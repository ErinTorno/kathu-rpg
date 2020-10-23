{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Verda.Graphics.Sprites
    ( SpriteID(..)
    , AnimationStrip(..)
    , Animation(..)
    , AnimatedSprite(..)
    , StaticSprite(..)
    , Sprite(..)
    , SurfaceVector
    , setAnimation
    , getAnimationID
    , setAnimationID
    , spriteID
    , spriteLayer
    , spriteScale
    , spriteRectangle
    , updateFrames
    ) where

import           Apecs
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types             (Parser, typeMismatch)
import qualified Data.Bifunctor               as Bi
import           Data.Function                (on)
import           Data.Functor.Compose
import qualified Data.HashMap.Strict          as HashMap
import           Data.List                    (sortBy)
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as T
import qualified Data.Vector                  as Vec
import qualified Data.Vector.Unboxed          as UVec
import           Data.Vector.Unboxed.Deriving
import           Data.Word
import           Foreign.C.Types              (CInt)
import           Linear.V2
import qualified SDL
import qualified SDL.Image                    as SDLI

import           Verda.IO.Directory
import           Verda.Parsing.Aeson
import           Verda.Parsing.Counting
import           Verda.Util.Dependency
import           Verda.Util.Flow
import           Verda.Util.Types

newtype SpriteID = SpriteID Int deriving (Show, Eq, Ord)

---------------
-- Animation --
---------------

data AnimationStrip = AnimationStrip
    { spriteRow  :: !Word32
    , frameCount :: !Word32
    , animDelay  :: !Word32
    }
derivingUnbox "AnimationStrip"
    [t| AnimationStrip -> (Word32, Word32, Word32) |]
    [| \(AnimationStrip row count delay) -> (row, count, delay) |]
    [| \(row, count, delay) -> AnimationStrip row count delay   |]

data Animation = Animation
    { animAtlas  :: !SpriteID
    , animBounds :: !(V2 CInt)
    , animStrips :: !(UVec.Vector AnimationStrip)
    , animIDs    :: !(IDMap CInt)
    }

-------------
-- Sprites --
-------------

data StaticSprite = StaticSprite
    { staticSprite :: !SpriteID
    , staticBounds :: !(SDL.Rectangle CInt)
    }

data AnimatedSprite = AnimatedSprite
    { animation     :: !Animation
    , activeAnim    :: !CInt
    , currentFrame  :: !CInt
    , animTime      :: !Word32
    }

data ScrollingSprite = ScrollingSprite
    { scrollSprite   :: !SpriteID
    , startBounds    :: !(SDL.Rectangle CInt)
    , boundsShift    :: !(V2 Double)
    , scrollDuration :: !CInt
    , scrollTime     :: !CInt
    }

data Sprite = AnimSprite !Double !Double !AnimatedSprite
            | StatSprite !Double !Double !StaticSprite
            | ScrlSprite !Double !Double !ScrollingSprite

instance Component Sprite where type Storage Sprite = Map Sprite

spriteLayer :: Sprite -> Double
spriteLayer (AnimSprite l _ _) = l
spriteLayer (StatSprite l _ _) = l
spriteLayer (ScrlSprite l _ _) = l

spriteScale :: Sprite -> Double
spriteScale (AnimSprite _ s _) = s
spriteScale (StatSprite _ s _) = s
spriteScale (ScrlSprite _ s _) = s

spriteID :: Sprite -> SpriteID
spriteID (StatSprite _ _ StaticSprite{staticSprite}) = staticSprite
spriteID (AnimSprite _ _ AnimatedSprite{animation = Animation{animAtlas}}) = animAtlas
spriteID (ScrlSprite _ _ ScrollingSprite{scrollSprite}) = scrollSprite

spriteRectangle :: Sprite -> SDL.Rectangle CInt
spriteRectangle (StatSprite _ _ StaticSprite{staticBounds}) = staticBounds
spriteRectangle (AnimSprite _ _ AnimatedSprite{activeAnim, currentFrame, animation = Animation{animBounds}}) = SDL.Rectangle (SDL.P pos) animBounds
    where pos = V2 currentFrame activeAnim * animBounds
spriteRectangle (ScrlSprite _ _ ScrollingSprite{startBounds, boundsShift = V2 fx fy, scrollTime, scrollDuration}) = SDL.Rectangle (SDL.P (V2 x' y')) dims
    where SDL.Rectangle (SDL.P (V2 sx sy)) dims = startBounds
          ratio = fromIntegral scrollTime / fromIntegral scrollDuration
          x' = floor $ (fx - fromIntegral sx) * ratio + fromIntegral sx
          y' = floor $ (fy - fromIntegral sy) * ratio + fromIntegral sy

updateFrames :: Word32 -> Sprite -> Sprite
updateFrames !dT sprite = case sprite of
    AnimSprite l s aspr@AnimatedSprite{animation, activeAnim, animTime} ->
        let newTime  = animTime + dT
            curStrip = animStrips animation UVec.! fromIntegral activeAnim
            newFrame = fromIntegral (newTime `quot` animDelay curStrip) `rem` frameCount curStrip
         in AnimSprite l s aspr{animTime = newTime, currentFrame = fromIntegral newFrame}
    ScrlSprite l s sspr@ScrollingSprite{scrollDuration, scrollTime} ->
        ScrlSprite l s sspr{scrollTime = (scrollTime + fromIntegral dT) `mod` scrollDuration}
    s -> s

-- | Updates a sprite to use the given animation, if it exists for that sprite. No-op for non-AnimatedSprite.
setAnimation :: Identifier -> Sprite -> Sprite
setAnimation !idt as@(AnimSprite _ _ animSpr) = case Map.lookup idt . animIDs . animation $ animSpr of
    Nothing  -> as
    Just idx -> setAnimationID idx as
setAnimation _ sprite = sprite

-- | Returns a sprite's current animation. A non-AnimatedSprite is considered to be permanently using animation index 0.
getAnimationID :: Sprite -> CInt
getAnimationID (AnimSprite _ _ anim) = activeAnim anim
getAnimationID _ = 0

-- | Updates a sprite to use the given animation ID. No bounds checking. No-op for non-AnimatedSprite.
setAnimationID :: CInt -> Sprite -> Sprite
setAnimationID idx (AnimSprite l s anim) = AnimSprite l s anim{activeAnim = idx, currentFrame = 0, animTime = timeBeforeFrameChange anim}
setAnimationID _ s = s

-- | Time in ms directly before one frame of an animation would switch to another.
timeBeforeFrameChange :: AnimatedSprite -> Word32
timeBeforeFrameChange !animspr = subtract 1 . animDelay . (UVec.!curAnim) . animStrips . animation $ animspr
    where curAnim = fromIntegral $ activeAnim animspr

-------------------
-- Serialization --
-------------------

type SurfaceVector = Vec.Vector (SDL.Surface, Maybe SDL.RenderScaleQuality)

boundsFromSpriteID :: (MonadIO m, s `CanProvide` SurfaceVector) => SpriteID -> Dependency s m (V2 CInt)
boundsFromSpriteID (SpriteID idx) =
    let getSurface (s, _ :: Maybe SDL.RenderScaleQuality) = s
     in provide
    >>= liftDependency . SDL.surfaceDimensions . getSurface . (Vec.! idx)

instance ( s `CanProvide` WorkingDirectory
         , s `CanStoreEach`  '[CountingIDs, SurfaceVector]
         , MonadIO m
         ) => FromJSON (Dependency s m SpriteID) where
    parseJSON = withText "SpriteID" $ \t ->
        let -- Kinda a hack for now; texture scaling can be given in the file's path
            scaleType = if | "@nearest." `T.isInfixOf` t -> Just SDL.ScaleNearest
                           | "@linear." `T.isInfixOf` t  -> Just SDL.ScaleLinear
                           | otherwise                   -> Nothing
            url   = fmap T.pack . resolveAssetPathDP . T.unpack $ t
            adder = do url'       <- url
                       image      <- liftDependency . SDLI.load . T.unpack $ url'
                       images     <- readStore
                       writeStore . Vec.snoc images $ (image, scaleType)
                       pure $ Vec.length images
         in pure $ SpriteID . fromIntegral <$> (url >>= lookupOrExecAndVerify adder "SpriteID")

instance FromJSON AnimationStrip where
    parseJSON = withObject "AnimationStrip" $ \v ->
        AnimationStrip <$> v .: "row" <*> v .: "frames" <*> v .: "delay"

instance (FromJSON (Dependency s m SpriteID), Monad m) => FromJSON (Dependency s m Animation) where
    parseJSON (Object v) =
        let animIDMap :: Parser [(Identifier, AnimationStrip)]
            animIDMap  = Map.assocs <$> v .: "animations"
            idMap      = Map.fromList <$> (Bi.second (fromIntegral . spriteRow) <<$>> animIDMap)
            strips     = UVec.fromList . sortBy (compare `on` spriteRow) . fmap snd <$> animIDMap
         in getCompose $ Animation
                     <$> v .:- "atlas"
                     <*> v .:^ "bounds"
                     <*> Compose (pure <$> strips)
                     <*> Compose (pure <$> idMap)
    parseJSON e = typeMismatch "Animation" e

instance ( s `CanProvide` SurfaceVector
         , FromJSON (Dependency s m SpriteID)
         , MonadIO m) => FromJSON (Dependency s m StaticSprite) where
    parseJSON s@(String _) =
        let sprID    = parseJSON s
            mkBounds = SDL.Rectangle (SDL.P $ V2 0 0)
         in StaticSprite <<$>> sprID <<*>> (fmap mkBounds . boundsFromSpriteID =<<< sprID)
    parseJSON e = typeMismatch "StaticSprite" e

instance ( FromJSON (Dependency s m StaticSprite)
         , FromJSON (Dependency s m Animation)
         , FromJSON (Dependency s m SpriteID)
         , Monad m
         ) => FromJSON (Dependency s m Sprite) where
    parseJSON s@(String _)   = StatSprite 0 1 <<$>> parseJSON s
    parseJSON obj@(Object v) =
        if | HashMap.member "animations" v -> do
                anim  <- parseJSON obj
                layer <- v .:? "layer" .!= 0
                scale <- v .:? "scale" .!= 1
                let mkAnimSpr a = AnimatedSprite a 0 0 0 
                pure (AnimSprite layer scale . mkAnimSpr <$> anim)
            | HashMap.member "scroll-origin" v -> do
                scrollSpr <- v .: "atlas"
                layer    <- v .:? "layer" .!= 0
                scale    <- v .:? "scale" .!= 1
                bounds   <- v .: "bounds"
                srcPos   <- v .: "scroll-origin"
                destPos  <- v .: "scroll-target"
                duration <- v .: "duration"
                let srcRect  = SDL.Rectangle (SDL.P srcPos) bounds
                    shift    = fromIntegral <$> (destPos - srcPos)
                    mkScroll spr = ScrollingSprite spr srcRect shift duration 0
                pure (ScrlSprite layer scale . mkScroll <$> scrollSpr)
            | otherwise -> do
                staticSpr <- v .: "atlas"
                layer <- v .:? "layer" .!= 0
                scale <- v .:? "scale" .!= 1
                spritePos    <- v .:? "position" .!= V2 0 0
                spriteBounds <- v .:? "bounds"
                let updateSpr spr = case spriteBounds of
                        Nothing  -> spr {staticBounds = SDL.Rectangle (SDL.P spritePos) prevBounds}
                        Just bnd -> spr {staticBounds = SDL.Rectangle (SDL.P spritePos) bnd}
                        where SDL.Rectangle _ prevBounds = staticBounds spr
                pure (StatSprite layer scale . updateSpr <$> staticSpr)
    parseJSON e              = typeMismatch "Sprite" e
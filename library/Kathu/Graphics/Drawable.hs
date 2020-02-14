{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Kathu.Graphics.Drawable where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Functor.Compose (getCompose)
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))

import Kathu.Parsing.Aeson
import Kathu.Util.Dependency
import Kathu.Util.Flow ((>>>=), (<$$>))
import Kathu.Util.Types

-- | A newtype wrapper around a function that can grab image dimension information as a Dependency
newtype ImageBounds m g = ImageBounds {unImageBounds :: g -> m (V2 CInt)}

-- a drawable that can change
data AnimationStrip = AnimationStrip
    { animID     :: !Identifier
    , frameCount :: {-# UNPACK #-} !Int
    , row        :: {-# UNPACK #-} !Int
    , delay      :: {-# UNPACK #-} !Word32
    } deriving (Show, Eq)

instance ToJSON AnimationStrip where
    toJSON (AnimationStrip anID frames rowNum delayMS) = object ["id" .= anID, "frames" .= frames, "row" .= rowNum, "delay" .= delayMS]
instance FromJSON AnimationStrip where
    parseJSON = withObject "AnimationStrip" $ \v -> AnimationStrip <$> v .: "id" <*> v .: "frames" <*> v .: "row" <*> v .: "delay"

---------------
-- Animation --
---------------

data Animation g = Animation
    { animAtlas  :: g
    , animStrips :: Vector AnimationStrip
    , animBounds :: V2 CInt
    } deriving (Show, Eq)

instance (FromJSON (Dependency s m g), Monad m) => FromJSON (Dependency s m (Animation g)) where
    parseJSON (Object v) = getCompose $ Animation
        <$> v .:~ "atlas"
        <*> v .:^ "strips"
        <*> v .:^ "bounds"
    parseJSON v = typeMismatch "Animation" v

data StaticSprite g = StaticSprite {staticSurface :: g, staticBounds :: V2 CInt} deriving (Show, Eq)

data AnimatedSprite g = AnimatedSprite
    { animation    :: Animation g
    , activeAnim   :: !Int
    , currentFrame :: {-# UNPACK #-} !Int
    , animTime     :: !Word32
    } deriving (Show, Eq)

------------------
-- RenderSprite --
------------------

data RenderSprite g = RSStatic (StaticSprite g) | RSAnimated (AnimatedSprite g) deriving (Show, Eq)

instance ( s `CanProvide` (ImageBounds (Dependency s m) g)
         , FromJSON (Dependency s m (Animation g))
         , FromJSON (Dependency s m g)
         , Monad m
         ) => FromJSON (Dependency s m (RenderSprite g)) where
    parseJSON s@(String _) = parseJSON s >>>= \graphics -> (\bnd -> RSStatic . flip StaticSprite bnd $ graphics) <$> bounds graphics
        where bounds :: (s `CanProvide` (ImageBounds (Dependency s m) g), Monad m) => g -> Dependency s m (V2 CInt)
              bounds graphics   = provide >>= ($graphics) . unImageBounds
    parseJSON o@(Object _) = parseJSON o >>>= \graphics -> pure . RSAnimated $ AnimatedSprite graphics 0 0 0 
    parseJSON v            = typeMismatch "RenderSprite" v

------------
-- Render --
------------

newtype Render g = Render {unRender :: Vector (RenderSprite g)}

instance (FromJSON (Dependency s m (RenderSprite g)), Monad m) => FromJSON (Dependency s m (Render g)) where
    parseJSON obj@(Object _) = Render . Vec.singleton <$$> parseJSON obj
    parseJSON str@(String _) = Render . Vec.singleton <$$> parseJSON str
    parseJSON (Array a)      = toRender <$> Vec.foldM run (pure []) a
        where run acc cur = (>>=(\inner -> (inner:) <$> acc)) <$> parseJSON cur
              toRender ls = Render <$> (Vec.fromList <$> ls)
    parseJSON e              = typeMismatch "Render" e

--------------------
-- Util Functions --
--------------------

-- we use this so that rapidly starting and stopping moving in one direction is still animated
timeBeforeFrameChange :: AnimatedSprite g -> Word32
timeBeforeFrameChange !animspr = subtract 1 . delay . (Vec.!curAnim) . animStrips . animation $ animspr
    where curAnim = activeAnim animspr

currentBounds :: RenderSprite g -> (# V2 CInt, V2 CInt #)
currentBounds (RSStatic (StaticSprite _ !bnd)) = (# V2 0 0, bnd #)
currentBounds (RSAnimated !anim) = (# V2 xCoord yCoord, dims #)
    where xCoord = (*w) . fromIntegral . currentFrame $ anim
          yCoord = (*h) . fromIntegral . activeAnim $ anim
          dims@(V2 !w !h) = animBounds . animation $ anim

isAnimated :: RenderSprite g -> Bool
isAnimated (RSStatic _)   = False
isAnimated (RSAnimated _) = True

switchAnimation :: Int -> AnimatedSprite g -> AnimatedSprite g
switchAnimation !i !anim = anim {activeAnim = i, currentFrame = 0, animTime = timeBeforeFrameChange anim}

switchAnimationByID :: Identifier -> AnimatedSprite g -> AnimatedSprite g
switchAnimationByID !idt !anim = case Vec.findIndex ((==idt) . animID) . animStrips . animation $ anim of
    Just idx -> switchAnimation idx anim
    Nothing  -> anim -- no switching if invalid is given; maybe should change this later?

-- updates current time, and switches to new frame if we reach it
updateFrames :: Word32 -> AnimatedSprite g -> AnimatedSprite g
updateFrames !dT d@AnimatedSprite {animTime = animT, activeAnim = act, animation = anim} = d {animTime = newTime, currentFrame = newFrame}
    where newTime  = animT + dT
          curStrip = animStrips anim Vec.! act
          newFrame = fromIntegral (newTime `quot` delay curStrip) `rem` frameCount curStrip
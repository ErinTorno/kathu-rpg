{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- The PaletteManager record has names for its components which arent supported, but clarify what they mean

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.Graphics.Palette
    ( AnimatedPalette(..)
    , Palette(..)
    , PaletteManager(..)
    , Shader(..)
    , StaticPalette(..)
    , applyAnimatedPalette
    , composeShaders
    , emptyPalette
    , allBackgrounds
    , staticManager
    , animatedManager
    , managerFromPalette
    ) where

import           Apecs
import           Control.Monad         (when)
import           Data.Aeson
import           Data.Aeson.Types      (Parser, typeMismatch)
import           Data.Foldable         (foldl')
import qualified Data.IntMap.Strict    as IntMap
import           Data.IntMap.Strict    (IntMap)
import qualified Data.Map              as Map
import qualified Data.Text             as T
import qualified Data.Vector           as Vec
import           Data.Word

import           Kathu.Entity.Time
import           Kathu.Graphics.Color
import           Kathu.Util.Types      (Identifier, mkIdentifier)
    
data StaticPalette = StaticPalette
    { background :: Color
    , shader     :: Shader
    }

data CycleEnd = Restart | Reverse | ChangeTo Identifier

data AnimatedPalette = AnimatedPalette
    { paletteKeyFrames :: IntMap StaticPalette
    , renderDelay      :: Word32
    , cycleDuration    :: Word32
    , cycleEnd         :: CycleEnd
    , colorInterpolate :: Double -> Color -> Color -> Color
    }

applyAnimatedPalette :: AnimatedPalette -> Color -> [Color]
applyAnimatedPalette apal col = interpolateAnimatedColors (\(StaticPalette _ (Shader s)) -> s col) apal

animatedPaletteBackgrounds :: AnimatedPalette -> [Color]
animatedPaletteBackgrounds = interpolateAnimatedColors background

interpolateAnimatedColors :: (StaticPalette -> Color) -> AnimatedPalette -> [Color]
interpolateAnimatedColors getCol (AnimatedPalette frames delay dur _ intrp) = fromFrame . getFrames <$> [0,delay..dur]
    where -- If it fails, something wrong was allowed to happen during the AnimatedPalette's creation
          fromFrame (Just c) = c
          fromFrame Nothing  = error "Unabled to determine next keyframe, do the keyframes end before the animation does?"
          colorFrames = getCol <$> frames
          getFrames :: Word32 -> Maybe Color
          getFrames t = do
              let tcur = fromIntegral t
              (ti, coli) <- IntMap.lookupLE tcur colorFrames
              (tf, colf) <- IntMap.lookupGE tcur colorFrames
              if ti == tf then -- if same time, we just return first, as they must be the same color and otherwise we'd divide by 0
                  pure coli
              else               -- blends between the two boundary colors found
                  pure $ intrp (fromIntegral (tcur - ti) / fromIntegral (tf - ti)) coli colf

data Palette = SPalette StaticPalette | APalette AnimatedPalette

emptyPalette :: Palette
emptyPalette = SPalette $ StaticPalette black (Shader id)

newtype Shader = Shader {unShader :: Color -> Color}

composeShaders :: Foldable f => f Shader -> Shader
composeShaders = Shader . foldl' (\acc -> (.acc) . unShader) id

allBackgrounds :: Palette -> [Color]
allBackgrounds (SPalette spal) = pure . background $ spal
allBackgrounds (APalette apal) = animatedPaletteBackgrounds apal

instance FromJSON CycleEnd where
    parseJSON (String "restart") = pure Restart
    parseJSON (String "reverse") = pure Reverse
    parseJSON (String s)         = case T.unpack s of
        ('c':'h':'a':'n':'g':'e':'-':'t':'o':' ':xs) -> pure $ ChangeTo (mkIdentifier . T.strip . T.pack $ xs)
        e                                            -> fail $ "Unknown CycleEnd type " ++ show e
    parseJSON e                  = typeMismatch "CycleEnd" e

instance FromJSON StaticPalette where
    parseJSON (Object v) = StaticPalette <$> v .: "background" <*> v .:? "shader" .!= Shader id
    parseJSON e          = typeMismatch "StaticPalette" e

instance FromJSON AnimatedPalette where
    parseJSON (Object v) = verify =<< AnimatedPalette
                       <$> (IntMap.fromList . Map.assocs <$> v .: "keyframes")
                       <*> ((1000`quot`) <$> (v .:? "frames-per-second" .!= 15)) -- 15 palette shifts per second by default
                       <*> v .: "cycle-duration"
                       <*> v .:? "cycle-end-behavior" .!= Reverse
                       <*> (intrp =<< v .:? "interpolation-fn" .!= "linear")
        where verify pal@AnimatedPalette {paletteKeyFrames = frames, cycleDuration = dur}
                  | IntMap.size frames < 2              = fail "Attempted to load AnimatedPalette with 0 or 1 keyframes"
                  | outOfDur dur . IntMap.keys $ frames = fail . concat $ ["AnimatedPalette contained keyframe with a time outside of the duration (", show dur, ")"]
                  | otherwise                           = pure pal
              outOfDur d = any (\t -> t < 0 || t > fromIntegral d)
              intrp :: T.Text -> Parser (Double -> Color -> Color -> Color)
              intrp "linear"  = pure blendColor
              intrp "angular" = pure $ \r c1 c2 -> fromHSV $ blendHSV (realToFrac r) (fromRGB c1) (fromRGB c2)
              intrp e         = fail $ "Unknown interpolation function " ++ show e
    parseJSON e          = typeMismatch "StaticPalette" e

instance FromJSON Palette where
    parseJSON o@(Object v) = v .:? "cycle-duration" >>= \case
        (Just (_ :: Int)) -> APalette <$> parseJSON o -- if there is a duration, this must be animated, so we parse as such
        Nothing           -> SPalette <$> parseJSON o -- otherwise we parse as static
    parseJSON e            = typeMismatch "Palette" e

instance FromJSON Shader where
    parseJSON (Array a)  = composeShaders <$> mapM parseJSON a
    parseJSON (Object v) = v .: "fn" >>= parseFn
        where parseFn str = fmap Shader $ case str of
                  "set-color"     -> const <$> v .: "color"
                  "desaturate"    -> pure desaturate
                  "desaturate-by" -> desaturateBy <$> (v .: "percent" :: Parser Double)
                  "blend-color"   -> blendColor <$> (v .: "percent" :: Parser Double) <*> v .: "color"
                  "shift-hue"     -> fromHSVFunction . shiftHue <$> v .: "angle"
                  "invert-hue"    -> pure . fromHSVFunction $ invertHue
                  "invert-rgb"    -> pure invertRGB
                  "match-nearest" -> nearestColor <$> (v .: "color-set" :: Parser (Vec.Vector Color))
                  "shift-hue-towards"     -> fromHSVFunction <$> (shiftHueTowards <$> v .: "angle" <*> v .: "percent")
                  "shift-hue-towards-abs" -> fromHSVFunction <$> (shiftHueTowardsAbs <$> v .: "target-angle" <*> v .: "shift-angle")
                  f               -> error $ "Attempted to parse unknown filter " ++ f
    parseJSON v = typeMismatch "Shader" v

--------------------------
-- Component Management --
--------------------------

data PaletteManager = PaletteManager
    { initManager :: forall w m. (Get w m PaletteManager, Has w m PaletteManager, Set w m PaletteManager, Get w m RenderTime, Has w m RenderTime)
                  => (Int -> SystemT w m ()) -> PaletteManager -> SystemT w m () -- we don't allow setting the PaletteManager in the init to prevent infinite loops
    , runManager  :: forall w m. (Get w m PaletteManager, Has w m PaletteManager, Set w m PaletteManager, Get w m RenderTime, Has w m RenderTime)
                  => (Int -> SystemT w m ()) -> (Identifier -> SystemT w m ()) -> PaletteManager -> SystemT w m ()
    }

data AnimatedPaletteState = AnimatedPaletteState
    { animPalette     :: {-# UNPACK #-} !AnimatedPalette
    , minPaletteIdx   :: !Int -- inclusive
    , maxPaletteIdx   :: !Int -- inclusive
    , isReversing     :: !Bool
    , currentFrame    :: !Int
    , lastChangeTime  :: !Word32
    }

staticManager :: Int -> PaletteManager
staticManager idx = PaletteManager (\setIdx _ -> setIdx idx) (\_ _ _ -> pure ())

animatedManager :: AnimatedPaletteState -> PaletteManager
animatedManager conf@(AnimatedPaletteState AnimatedPalette {renderDelay = delay, cycleEnd = cycEnd} minIdx maxIdx isRev curPalette lastTime) = PaletteManager initi run
    where initi changeFrame _ = changeFrame minIdx
          run (changeFrame :: Int -> SystemT w m ()) changePalette _ = do
              RenderTime curTime <- get global
              when (curTime - lastTime >= fromIntegral delay) $ do
                  let nextPalette = if isRev then curPalette - 1 else curPalette + 1

                  if nextPalette < minIdx then do
                      -- can't reach this unless reversing, so we know to stop and start running the other direction
                      changeFrame (curPalette + 1)
                      -- we use curTime instead of lastTime + delay, since we don't care about playing "catch-up" with this animation
                      global $= animatedManager (conf {isReversing = False, currentFrame = curPalette + 1, lastChangeTime = curTime})
                  else if nextPalette > maxIdx then case cycEnd of
                      Restart      -> do
                          changeFrame minIdx
                          global $= animatedManager (conf {currentFrame = minIdx, lastChangeTime = curTime})
                      Reverse      -> do
                          changeFrame (maxIdx - 1)
                          global $= animatedManager (conf {isReversing = True, currentFrame = maxIdx - 1, lastChangeTime = curTime})
                      ChangeTo idt ->
                          changePalette idt
                  else do
                      -- normal behavior, change and move on
                      changeFrame nextPalette
                      global $= animatedManager (conf {currentFrame = nextPalette, lastChangeTime = curTime})

managerFromPalette :: Int -> Int -> Palette -> PaletteManager
managerFromPalette i _ (SPalette _) = staticManager i
managerFromPalette minIdx maxIdx (APalette ap) = animatedManager aps
    where aps = AnimatedPaletteState ap minIdx maxIdx False minIdx 0
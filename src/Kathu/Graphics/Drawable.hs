module Kathu.Graphics.Drawable where

import Data.Text (Text)
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Foreign.C.Types (CInt)
import qualified Kathu.Util.SDLCommon as SDLC
import qualified SDL

type Image = SDL.Surface
newtype ImageID = ImageID Int deriving (Show, Eq, Ord)

data AnimStyle = Single | StandardActor deriving (Show, Eq)

-- a drawable that can change
data AnimationStrip = AnimationStrip {animID :: Text, frameCount :: Int, row :: Int, delay :: Word32} deriving (Show, Eq)

data Animation = Animation
    { animAtlas  :: ImageID
    , animStrips :: Vector AnimationStrip
    , animBounds :: SDL.V2 CInt
    } deriving (Show, Eq)

data StaticSprite = StaticSprite {staticSurface :: ImageID, staticBounds :: SDL.Rectangle CInt} deriving (Show, Eq)

data AnimatedSprite = AnimatedSprite
    { animation    :: Animation
    , activeAnim   :: Int
    , currentFrame :: Int
    , animTime     :: Word32
    } deriving (Show, Eq)

data RenderSprite = RSStatic StaticSprite | RSAnimated AnimatedSprite deriving (Show, Eq)

-- we use this so that rapidly starting and stopping moving in one direction is still animated
timeBeforeFrameChange :: AnimatedSprite -> Word32
timeBeforeFrameChange animspr = (subtract 1) . delay . (Vec.!curAnim) . animStrips . animation $ animspr
    where curAnim = activeAnim animspr

currentBounds :: RenderSprite -> SDL.Rectangle CInt
currentBounds (RSStatic (StaticSprite _ bnd)) = bnd
currentBounds (RSAnimated anim) = SDLC.mkRect ((*) w . fromIntegral . currentFrame $ anim) ((*) h . fromIntegral . activeAnim $ anim) w h
    where (SDL.V2 w h) = animBounds . animation $ anim

isAnimated :: RenderSprite -> Bool
isAnimated (RSStatic _)   = False
isAnimated (RSAnimated _) = True

getImageID :: RenderSprite -> ImageID
getImageID (RSStatic (StaticSprite img _)) = img
getImageID (RSAnimated anim) = animAtlas . animation $ anim

switchAnimation :: Int -> AnimatedSprite -> AnimatedSprite
switchAnimation i anim = anim {activeAnim = i, currentFrame = 0, animTime = 0}

-- updates current time, and switches to new frame if we reach it
updateFrames :: Word32 -> AnimatedSprite -> AnimatedSprite
updateFrames dT d@(AnimatedSprite {animTime = animT, activeAnim = act, currentFrame = frame, animation = anim}) = d {animTime = newTime, currentFrame = newFrame}
    where newTime  = animT + dT
          curStrip = (animStrips anim) Vec.! act
          newFrame = fromIntegral (newTime `quot` (delay curStrip)) `rem` (frameCount curStrip)
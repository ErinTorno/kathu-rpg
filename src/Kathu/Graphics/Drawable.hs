module Kathu.Graphics.Drawable where

import Data.Text (Text)
import Data.Word
import Data.Vector (Vector, (!))
import Foreign.C.Types (CInt)
import qualified Kathu.SDLCommon as SDLC
import qualified SDL

type Image = SDL.Surface

data AnimStyle = Single | StandardActor deriving (Show, Eq)

-- a drawable that can change
data AnimationStrip = AnimationStrip {animID :: Text, frameCount :: Int, row :: Int, delay :: Word32} deriving (Show, Eq)

data Animation = Animation
    { animAtlas  :: Image
    , animStrips :: Vector AnimationStrip
    , animBounds :: SDL.V2 CInt
    }

data StaticSprite = StaticSprite {staticSurface :: Image, staticBounds :: SDL.Rectangle CInt}
data AnimatedSprite = AnimatedSprite
    { animation    :: Animation
    , activeAnim   :: Int
    , currentFrame :: Int
    , animTime     :: Word32
    }

data RenderSprite = RSStatic StaticSprite | RSAnimated AnimatedSprite

currentBounds :: RenderSprite -> SDL.Rectangle CInt
currentBounds (RSStatic (StaticSprite _ bnd)) = bnd
currentBounds (RSAnimated anim) = SDLC.mkRect ((*) w . fromIntegral . currentFrame $ anim) ((*) h . fromIntegral . activeAnim $ anim) w h
    where (SDL.V2 w h) = animBounds . animation $ anim

isAnimated :: RenderSprite -> Bool
isAnimated (RSStatic _)   = False
isAnimated (RSAnimated _) = True

getImage :: RenderSprite -> Image
getImage (RSStatic (StaticSprite img _)) = img
getImage (RSAnimated anim) = animAtlas . animation $ anim

switchAnimation :: Int -> AnimatedSprite -> AnimatedSprite
switchAnimation i anim = anim {activeAnim = i, currentFrame = 0, animTime = 0}

-- updates current time, and switches to new frame if we reach it
updateFrames :: Word32 -> AnimatedSprite -> AnimatedSprite
updateFrames dT d@(AnimatedSprite {animTime = animT, activeAnim = act, currentFrame = frame, animation = anim}) = d {animTime = newTime, currentFrame = newFrame}
    where newTime  = animT + dT
          curStrip = (animStrips anim) ! act
          newFrame = fromIntegral (newTime `quot` (delay curStrip)) `rem` (frameCount curStrip)
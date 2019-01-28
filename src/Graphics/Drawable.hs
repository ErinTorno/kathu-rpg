module Graphics.Drawable where

import Data.Text (Text)
import Data.Word
import Data.Vector (Vector, (!))
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDLCommon as SDLC

-- required to implement, so when we use it we just assume the rest of the animations other than the surface must be consistant
instance Eq SDL.Surface where (==) _ _ = True
instance Show SDL.Surface where show _ = "Loaded Surface"

data AnimStyle = Single | StandardActor deriving (Show, Eq)

-- a drawable that can change
data AnimationStrip = AnimationStrip {animID :: Text, frameCount :: Int, row :: Int, delay :: Word32} deriving (Show, Eq)

data Animation = Animation
    { animAtlas  :: SDL.Surface
    , animStrips :: Vector AnimationStrip
    , animBounds :: SDL.V2 CInt
    } deriving (Show, Eq)

data RenderSprite
    = StaticSprite {staticSurface :: SDL.Surface, staticBounds :: SDL.Rectangle CInt}
    | AnimatedSprite
      { animation    :: Animation
      , activeAnim   :: Int
      , currentFrame :: Int
      , animTime     :: Word32
      } deriving (Show, Eq)

currentBounds :: RenderSprite -> SDL.Rectangle CInt
currentBounds (StaticSprite _ bnd) = bnd
currentBounds sprite = SDLC.mkRect ((*) w . fromIntegral . currentFrame $ sprite) ((*) h . fromIntegral . activeAnim $ sprite) w h
    where (SDL.V2 w h) = animBounds . animation $ sprite

isAnimated StaticSprite {} = False
isAnimated _               = True

-- updates current time, and switches to new frame if we reach it
updateFrames dT s@(StaticSprite {})   = s
updateFrames dT d@(AnimatedSprite {animTime = animT, activeAnim = act, currentFrame = frame, animation = anim}) = d {animTime = newTime, currentFrame = newFrame}
    where newTime  = animT + dT
          curStrip = (animStrips anim) ! act
          newFrame = fromIntegral (newTime `quot` (delay curStrip)) `rem` (frameCount curStrip)
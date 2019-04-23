{-# LANGUAGE BangPatterns #-}

module Kathu.Render where

import Apecs hiding (($=))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor
import qualified Data.Vector as Vec
import Data.Word
import Foreign.C.Types (CInt)
import Graphics.Rendering.OpenGL hiding (get, Render)
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Graphics.Drawable
import Kathu.IO.Settings
import qualified Kathu.SDLCommon as SDLC
import Kathu.Util
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import qualified SDL
import SDL (($=))

{-
type Color = SDL.V4 Word8
backgroundColor :: Color
backgroundColor = SDL.V4 0 0 0 maxBound
-}

-- the camera will render excepting the camera to look down on us at these degrees
angleFromFront :: Float
angleFromFront = tan $ 215.0 * (2.0 * pi / 180.0)

-- the camera itself is 10.0 units tall (although due to perspective, shown might be different)
unitsPerHeight :: Float
unitsPerHeight = 10.0

pixelsPerUnit :: Float
pixelsPerUnit = 16.0

aspectRatio :: Fractional a => SDL.V2 a -> a
aspectRatio (SDL.V2 x y) = x / y

{-
isWithinCamera :: SDL.V2 Float -> SDL.V3 Float -> SDL.V3 Float -> Bool
isWithinCamera (SDL.V2 camW camH) (SDL.V3 cx _ cz) (SDL.V3 tx ty tz) = withinX && withinZY
    where zdepth   = ty * angleFromFront
          withinZY = zdepth - camH <= tz && tz <= zdepth + camH
          withinX  = cx - camW <= tx && tx <= cx + camW
-}

{-
-- scales a render image's rectangle to wherever it should be when drawn
-- as of now, we don't scale for z at all
getRenderRect :: V2 Float -> V2 Float -> Float -> SDL.Rectangle CInt -> SDL.Rectangle CInt
getRenderRect (V2 topX topY) (V2 tarX tarY) scale (SDL.Rectangle _ (V2 w h)) = fmap floor rect
    where rect = SDLC.mkRect (tarX - topX) (tarY - topY) (scale * fromIntegral w) (scale * fromIntegral h)

-- render component

centerRect :: SDL.Rectangle Float -> SDL.Rectangle Float
centerRect (SDL.Rectangle (SDL.P (V2 x y)) dim) = SDL.Rectangle (SDL.P (V2 (x / 2.0) (y / 2.0))) dim

drawRenderSprite :: MonadIO m => SDL.Surface -> (SDL.Rectangle CInt -> SDL.Rectangle CInt) -> RenderSprite -> m ()
drawRenderSprite scr scaling (StaticSprite !img !bnd) = SDL.surfaceBlitScaled img (Just bnd) scr (Just . scaling $ bnd)
drawRenderSprite scr scaling dyn@(AnimatedSprite {animation = anim}) = SDL.surfaceBlitScaled (animAtlas anim) (Just bounds) scr (Just . scaling $ bounds)
    where bounds = currentBounds dyn

updateAnimations :: Word32 -> System' ()
updateAnimations dT = do
    -- anim frames are updated
    cmap $ \(Render spr) -> Render . Vec.map (updateFrames dT) $ spr

{-
updateAnimations :: System' ()
updateAnimations = do
    -- anim frames are updated
    cmap $ \(Render spr) -> Render . Vec.map (updateFrames dT) $ spr
    let updateCurAnim :: (Render, ActionSet) -> Render
        updateAnimations (Render spr, ActionSet{moving = m, lastMoving = lm}) = if m == lm then Render spr else switch
            where switch 
-}
-}
-- main render loop

{-
runRender :: SDL.Window -> Word32 -> SystemT' IO ()
runRender !window !dT = do
    do stepRenderTime dT

    screen   <- SDL.getWindowSurface window
    settings <- get global
    
    -- clears background
    SDL.surfaceFillRect screen Nothing backgroundColor

    updateAnimations dT

    let -- aspect  = aspectRatio . fmap fromIntegral . resolution $ settings
        scale   = (fromIntegral . resolutionY $ settings) / (pixelsPerUnit * unitsPerHeight)
        (SDL.V2 resX resY) = fromIntegral <$> resolution settings
        -- camEdge = SDL.V2 (unitsPerHeight * aspect) unitsPerHeight
        runForCamera :: SDL.Surface -> (Camera, Position) -> SystemT' IO SDL.Surface
        runForCamera scr (cam, Position (V3 x y _)) = cfoldM (\s (Position (V3 px py 0), Render sprs) -> Vec.foldM (draw $ V2 px py) s sprs) scr
            where topLeft = V2 (scale * x - 0.5 * resX) (scale * y - 0.5 * resY)
                  draw p scr sprite = drawRenderSprite scr (getRenderRect topLeft ((*scale) <$> p) scale) sprite $> scr
    cfoldM_ runForCamera screen
    
    lift $ runRenderGL window

    SDL.updateWindowSurface window
-}

runRender :: SDL.Window -> Word32 -> SystemT' IO ()
runRender window dT = do
    do stepRenderTime dT
    settings <- get global
    let startGL :: IO ()
        startGL = do
            clearColor $= Color4 0 0 0 1
            clear [ColorBuffer]
                    -- error . concat $ ["x ", show x, " y ", show y, " z ", show z]
        endGL :: IO ()
        endGL = do
            flush
            SDL.glSwapWindow window
        drawSprite sprite = do
            let tex = getImage sprite
            SDL.glBindTexture tex
            let mkQuad :: GLfloat -> [Vertex3 GLfloat]
                mkQuad w = [Vertex3 (-w) (-w) 0, Vertex3 (-w) w 0, Vertex3 w w 0, Vertex3 w (-w) 0]
            -- 1.0 top of screen, -1.0 bottom, 1.0 right, -1.0 left
            preservingMatrix $ do
                color $ Color3 0 0 (0 :: GLfloat)
                renderPrimitive Quads $ mapM_ vertex (mkQuad 0.1)
            SDL.glUnbindTexture tex
        -- Apecs related drawing
        aspect = aspectRatio . fmap fromIntegral . resolution $ settings
        (SDL.V2 resX resY) = fromIntegral <$> resolution settings
        renderEachSprite (Render spr) = lift . forM_ spr $ drawSprite
    lift startGL
    -- draw each quad
    cmapM_ renderEachSprite
    lift endGL
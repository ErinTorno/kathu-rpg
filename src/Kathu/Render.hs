{-# LANGUAGE BangPatterns #-}

module Kathu.Render where

import Apecs hiding (($=))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Word
import Foreign.C.Types (CInt)
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Graphics.Camera
import Kathu.Graphics.Color
import Kathu.Graphics.Drawable
import Kathu.Graphics.ImageManager
import Kathu.Graphics.RenderBuffer
import Kathu.IO.Settings
import Kathu.World.WorldSpace
import qualified Kathu.Util.SDLCommon as SDLC
import Kathu.Util.Misc
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import qualified SDL

cameraAngle :: Floating a => a
cameraAngle = 35.0

cameraZMult :: Floating a => a
cameraZMult = sin cameraAngle

-- the height of the screen in units; depending on screen size, more or less is included

minUnitsPerHeight :: Floating a => a
minUnitsPerHeight = 8.0

maxUnitsPerHeight :: Floating a => a
maxUnitsPerHeight = 14.0

pixelsForMinUnits :: Integral a => a
pixelsForMinUnits = 360

pixelsForMaxUnits :: Integral a => a
pixelsForMaxUnits = 1080


-- if sprite position is more than this many units from left or right, or from bottom, we don't draw
-- we don't draw anything above the top of the screen, however, since sprites draw out and upwards
renderBorderUnits :: Floating a => a
renderBorderUnits = 4.0

pixelsPerUnit :: Floating a => a
pixelsPerUnit = 16.0

aspectRatio :: Floating a => SDL.V2 a -> a
aspectRatio (SDL.V2 x y) = x / y

-- sprite dimensions are multiplied by this to prevent tiny streaks between adjacent sprites
edgeBleedScaling :: Floating a => a
edgeBleedScaling = 1.01

logicCoordToRender :: Floating a => a -> V3 a -> V3 a -> V3 a
logicCoordToRender scale (V3 topX topY topZ) (V3 tarX tarY tarZ) = V3 x' y' z'
    where x' = (tarX - topX) * scale
          -- this ensures that the z angle is factored into where it appears
          y' = (tarY - topY + cameraZMult * (tarZ - topZ)) * scale
          z' = tarY - topY -- used only for sorting, closer along y coord is only consideration


mkRenderRect :: V2 Float -> Float -> V2 Float -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRect (V2 shiftX shiftY) scale (V2 x y) (SDL.Rectangle _ (V2 w h)) = SDLC.mkRectWith round x' y' (edgeBleedScaling * scale * fromIntegral w) (edgeBleedScaling * scale * fromIntegral h)
    where x' = x - scale * 0.5 * fromIntegral w + shiftX
          y' = y - scale * fromIntegral h + shiftY

drawRenderSprite :: MonadIO m => ImageManager -> (SDL.Rectangle CInt -> SDL.Rectangle CInt) -> RenderSprite -> SDL.Surface -> m SDL.Surface
drawRenderSprite im mkRect ren scr = blit ren >> pure scr
    where draw bnd img = SDL.surfaceBlitScaled img (Just bnd) scr (Just . mkRect $ bnd)
          blit (RSStatic (StaticSprite !iid !bnd)) = fetchImage iid im >>= draw bnd
          blit dyn@(RSAnimated (AnimatedSprite {animation = anim})) = fetchImage (animAtlas anim) im >>= draw bounds
              where bounds = currentBounds dyn

updateAnimations :: Word32 -> System' ()
updateAnimations dT = do
    -- anim frames are updated
    let updateFramesIfAnim s@(RSStatic _)    = s
        updateFramesIfAnim (RSAnimated anim) = RSAnimated $ updateFrames dT anim
        updateWithoutController :: (Render, Not ActionSet) -> Render
        updateWithoutController (Render sprites, _) = Render $ updateFramesIfAnim <$> sprites
        updateAnimated :: (Render, ActionSet) -> Render
        updateAnimated ((Render sprites), ActionSet {_moving = m, _lastMoving = lm, _facingDirection = fac}) = Render $ updateEach <$> sprites
            where updateEach s@(RSStatic _)    = s
                  updateEach (RSAnimated anim) = RSAnimated $ update m lm fac anim
                  update Nothing _ fc anim = anim {activeAnim = dirToAnimIndex fc, currentFrame = 0, animTime = timeBeforeFrameChange anim} -- we ensure that this is paused and waiting on first frame
                  update mv@(Just d) lmv _ anim
                      | mv == lmv || dir == act = updateFrames dT anim -- if same direction, we just update its frames
                      | otherwise               = switchAnimation dir anim -- we switch to new animation and reset
                          where dir = dirToAnimIndex d
                                act = activeAnim anim
    -- only update frames for those without any controller for them
    cmap updateWithoutController
    -- if it has an ActionSet, we have to deal with swapping animations
    cmap updateAnimated

----------------------
-- main render loop --
----------------------

-- Dangerous function
-- Uses a lot of unsafe operations for efficiency, be very careful with code in here
runRender :: SDL.Window -> RenderBuffer -> Word32 -> SystemT' IO RenderBuffer
runRender !window !renBuf !dT = do
    do stepRenderTime dT
    updateAnimations dT

    screen   <- SDL.getWindowSurface window
    settings <- get global
    -- world <- get global
    imageManager <- get global

    -- clears background
    let (Color bgColor) = backgroundColor imageManager
    SDL.surfaceFillRect screen Nothing bgColor

    let scale   = (fromIntegral . resolutionY $ settings) / (pixelsPerUnit * unitsPerHeight)
        (V2 _ resY) = resolution settings
        resToCenter = ((*) 0.5 . fromIntegral) <$> resolution settings
        unitsPerHeight = minUnitsPerHeight + (maxUnitsPerHeight - minUnitsPerHeight) * pixMult
            where pixMult = fromIntegral (clampBetween pixelsForMinUnits pixelsForMaxUnits resY) / fromIntegral pixelsForMaxUnits
        unitsPerWidth = unitsPerHeight * aspectRatio resToCenter
        camShiftUp = 12
        -- this collects all renders into our buffer with their positions
        -- this takes transformed V3, rather than logical, since z is fully depth, rather than up down
        gatherRender :: (Int, RenderBuffer) -> (Camera, Position) -> SystemT' IO (Int, RenderBuffer)
        gatherRender (i, buf) (Camera zoom, Position (V3 camX camY camZ)) =
            let cam'@(V3 camX' camY' _) = V3 camX (camY - camShiftUp) camZ
                convPos = logicCoordToRender (scale * zoom) cam'
                minY = camY' + ((-0.5) * unitsPerHeight) * pixelsPerUnit
                maxY = camY' + (0.5    * unitsPerHeight + renderBorderUnits) * pixelsPerUnit
                minX = camX' + ((-0.5) * unitsPerWidth  - renderBorderUnits) * pixelsPerUnit
                maxX = camX' + (0.5    * unitsPerWidth  + renderBorderUnits) * pixelsPerUnit
                isOffScreen (V3 x y z) = y < minY || y > maxY || x < minX || x > maxX
            in (flip cfoldM) (i, buf) $ \(!i, !renBuf) (render, Position pos) -> if isOffScreen pos then pure (i, renBuf) else do
                -- in future, we won't draw off screen objects
                let renderPos = convPos pos
                    shouldDraw = True
                renBuf' <- lift $ growMVecIfNeeded bufferGrowIncr i buf
                if shouldDraw then
                    (lift . MVec.unsafeWrite renBuf i) (renderPos, render)
                    >> pure (i + 1, renBuf)
                else pure (i, renBuf)
        renderEvery :: Int -> Int -> RenderBuffer -> SDL.Surface -> IO ()
        renderEvery !i !len !buf !sur | i == len  = pure ()
                                      | otherwise = MVec.unsafeRead buf i >>= drawRender >> renderEvery (i + 1) len buf sur
            where drawRender (V3 x y _, Render sprs) = Vec.foldM_ (drawEach $ V2 x y) sur sprs
                  drawEach pos scr ren = drawRenderSprite imageManager (mkRenderRect resToCenter scale pos) ren scr

    (sprCount, renBuf') <- cfoldM gatherRender (0, renBuf)
    if sprCount > 0 then
        lift $ sortRenderBuffer 0 sprCount renBuf' >> renderEvery 0 sprCount renBuf' screen
    else pure ()

    SDL.updateWindowSurface window
    pure renBuf'
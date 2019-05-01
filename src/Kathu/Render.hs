{-# LANGUAGE BangPatterns #-}

module Kathu.Render where

import Apecs hiding (($=))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.ST (RealWorld)
import Data.Functor
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Word
import Foreign.C.Types (CInt)
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Graphics.Color
import Kathu.Graphics.Drawable
import Kathu.Graphics.RenderBuffer
import Kathu.IO.Settings
import qualified Kathu.SDLCommon as SDLC
import Kathu.Util
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import qualified SDL
import SDL (($=))

cameraAngle :: Floating a => a
cameraAngle = 35.0

cameraZMult :: Floating a => a
cameraZMult = sin cameraAngle

-- the camera itself is 10.0 units tall (although due to perspective, shown might be different)
unitsPerHeight :: Floating a => a
unitsPerHeight = 14.0

-- if sprite position is more than this many units from left or right, or from bottom, we don't draw
-- we don't draw anything above the top of the screen, however, since sprites draw out and upwards
renderBorderUnits :: Floating a => a
renderBorderUnits = 4.0

pixelsPerUnit :: Floating a => a
pixelsPerUnit = 16.0

cameraShiftUpPx :: Floating a => a
cameraShiftUpPx = -(0.5 * spriteHeightPx)
    where spriteHeightPx = 32.0

aspectRatio :: Floating a => SDL.V2 a -> a
aspectRatio (SDL.V2 x y) = x / y

logicCoordToRender :: Floating a => a -> V3 a -> V3 a -> V3 a
logicCoordToRender scale (V3 topX topY topZ) (V3 tarX tarY tarZ) = V3 x' y' z'
    where topY' = cameraShiftUpPx + topY
          x' = (tarX - topX) * scale
          -- this ensures that the z angle is factored into where it appears
          y' = (tarY - topY' + cameraZMult * (tarZ - topZ)) * scale
          z' = tarY - topY' -- used only for sorting, closer along y coord is only consideration


mkRenderRect :: V2 Float -> Float -> V2 Float -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRect (V2 shiftX shiftY) scale (V2 x y) (SDL.Rectangle _ (V2 w h)) = SDLC.mkRectWith floor x' y' (scale * fromIntegral w) (scale * fromIntegral h)
    where x' = x - scale * 0.5 * fromIntegral w + shiftX
          y' = y - scale * fromIntegral h + shiftY

drawRenderSprite :: MonadIO m => (SDL.Rectangle CInt -> SDL.Rectangle CInt) -> RenderSprite -> SDL.Surface -> m SDL.Surface
drawRenderSprite mkRect ren scr = blit ren >> pure scr
    where blit (RSStatic (StaticSprite !img !bnd)) = SDL.surfaceBlitScaled img (Just bnd) scr (Just . mkRect $ bnd)
          blit dyn@(RSAnimated (AnimatedSprite {animation = anim})) = draw
              where bounds = currentBounds dyn
                    draw   = SDL.surfaceBlitScaled (animAtlas anim) (Just bounds) scr (Just . mkRect $ bounds)

updateAnimations :: Word32 -> System' ()
updateAnimations dT = do
    -- anim frames are updated
    let updateFramesIfAnim s@(RSStatic _)    = s
        updateFramesIfAnim (RSAnimated anim) = RSAnimated $ updateFrames dT anim
        updateWithoutController :: (Render, Not ActionSet) -> Render
        updateWithoutController (Render sprites, _) = Render $ updateFramesIfAnim <$> sprites
        updateAnimations :: (Render, ActionSet) -> Render
        updateAnimations (ren@(Render sprites), ActionSet {_moving = m, _lastMoving = lm}) = Render $ updateEach <$> sprites
            where updateEach s@(RSStatic _)      = s
                  updateEach a@(RSAnimated anim) = RSAnimated $ update m lm anim
                  update Nothing _ anim = anim {currentFrame = 0, animTime = 0} -- we ensure that this is paused and waiting on first frame
                  update m@(Just d) lm anim
                      | m == lm   = updateFrames dT anim -- if same direction, we just update its frames
                      | otherwise = switchAnimation (dirToAnimIndex d) anim -- we switch to new animation and reset
    -- only update frames for those without any controller for them
    cmap updateWithoutController
    -- if it has an ActionSet, we have to deal with swapping animations
    cmap updateAnimations

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
    (BackgroundColor (Color bckColor)) <- get global
    
    -- clears background
    SDL.surfaceFillRect screen Nothing bckColor

    let scale   = (fromIntegral . resolutionY $ settings) / (pixelsPerUnit * unitsPerHeight)
        resToCenter@(V2 resX resY) = ((*) 0.5 . fromIntegral) <$> resolution settings
        aspect = aspectRatio . fmap fromIntegral . resolution $ settings
        unitsPerWidth = aspect * unitsPerHeight
        -- this collects all renders into our buffer with their positions
        -- this takes transformed V3, rather than logical, since z is fully depth, rather than up down
        gatherRender :: (Int, RenderBuffer) -> (Camera, Position) -> SystemT' IO (Int, RenderBuffer)
        gatherRender (i, buf) (cam, Position camera) =
            let convPos = logicCoordToRender scale
            in (flip cfoldM) (i, buf) $ \(!i, !renBuf) (render, Position pos) -> do
                -- in future, we won't draw off screen objects
                let renderPos = convPos camera pos
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
                  drawEach pos scr ren = drawRenderSprite (mkRenderRect resToCenter scale pos) ren scr

    (sprCount, renBuf') <- cfoldM gatherRender (0, renBuf)
    if sprCount > 0 then
        lift $ sortRenderBuffer 0 sprCount renBuf' >> renderEvery 0 sprCount renBuf' screen
    else pure ()

    SDL.updateWindowSurface window
    pure renBuf'
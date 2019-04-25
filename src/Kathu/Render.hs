{-# LANGUAGE BangPatterns #-}

module Kathu.Render where

import Apecs hiding (($=))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.ST (RealWorld)
import Data.Functor
import qualified Data.Vector as Vec
import Data.Vector.Algorithms.Intro (sortByBounds)
import qualified Data.Vector.Mutable as MVec
import Data.Word
import Foreign.C.Types (CInt)
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

------------------
-- RenderBuffer --
------------------

-- this stores each sprite to draw and its properties, which we will sort before drawing for z depth
type RenderBuffer = MVec.IOVector (V3 Float, Render)

mkRenderBuffer :: IO RenderBuffer
mkRenderBuffer = MVec.new 64

growRenderBuffer :: RenderBuffer -> IO RenderBuffer
growRenderBuffer = flip MVec.unsafeGrow $ bufferGrowIncr

-- whenever we outgrow the RenderBuffer, we grow it by this increment
bufferGrowIncr :: Int
bufferGrowIncr = 16

-- Color

type Color = SDL.V4 Word8
backgroundColor :: Color
backgroundColor = SDL.V4 0 0 0 maxBound

cameraAngle :: Floating a => a
cameraAngle = 35.0

{-
-- and we mult the logical y by it, then add logical z
-- removed for now, as multiplying the y leads to a bizarre feeling result
cameraYMult :: Floating a => a
cameraYMult = tan $ cameraAngle * (pi / 180.0)
-}

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
          -- y' = ((tarZ - topZ) + cameraYMult * (tarY - topY')) * scale -- this factors in both z and y coords
          y' = (tarY - topY' + cameraZMult * (tarZ - topZ)) * scale -- don't want to mult y, as then it appears as if we move slower in the y direction than the x
          z' = tarY - topY' -- used only for sorting, closer along y coord is only consideration

mkRenderRect :: V2 Float -> Float -> V2 Float -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRect (V2 shiftX shiftY) scale (V2 x y) (SDL.Rectangle _ (V2 w h)) = SDLC.mkRectWith floor x' y' (scale * fromIntegral w) (scale * fromIntegral h)
    where x' = x - scale * 0.5 * fromIntegral w + shiftX
          y' = y - scale * fromIntegral h + shiftY

drawRenderSprite :: MonadIO m => (SDL.Rectangle CInt -> SDL.Rectangle CInt) -> RenderSprite -> SDL.Surface -> m SDL.Surface
drawRenderSprite mkRect ren scr = blit ren >> pure scr
    where blit (StaticSprite !img !bnd) = SDL.surfaceBlitScaled img (Just bnd) scr (Just . mkRect $ bnd)
          blit dyn@(AnimatedSprite {animation = anim}) = SDL.surfaceBlitScaled (animAtlas anim) (Just bounds) scr (Just . mkRect $ bounds)
              where bounds = currentBounds dyn

{-
updateAnimations :: System' ()
updateAnimations = do
    -- anim frames are updated
    cmap $ \(Render spr) -> Render . Vec.map (updateFrames dT) $ spr
    let updateCurAnim :: (Render, ActionSet) -> Render
        updateAnimations (Render spr, ActionSet{moving = m, lastMoving = lm}) = if m == lm then Render spr else switch
            where switch
-}
-- main render loop

-- Highly dangerous function
-- Uses a lot of unsafe operations for efficiency, be very careful with code in here
runRender :: SDL.Window -> RenderBuffer -> Word32 -> SystemT' IO RenderBuffer
runRender !window !renBuf !dT = do
    do stepRenderTime dT

    screen   <- SDL.getWindowSurface window
    settings <- get global
    
    -- clears background
    SDL.surfaceFillRect screen Nothing backgroundColor

    -- updateAnimations dT

    let scale   = (fromIntegral . resolutionY $ settings) / (pixelsPerUnit * unitsPerHeight)
        resToCenter@(V2 resX resY) = ((*) 0.5 . fromIntegral) <$> resolution settings
        -- this collects all renders into our buffer with their positions
        -- this takes transformed V3, rather than logical, since z is fully depth, rather than up down
        sortRenders ((V3 _ _ za), _) ((V3 _ _ zb), _) = za `compare` zb
        gatherRender :: (Int, RenderBuffer) -> (Camera, Position) -> SystemT' IO (Int, RenderBuffer)
        gatherRender (i, buf) (cam, Position camera) =
            let convPos = logicCoordToRender scale            
            in (flip cfoldM) (i, buf) $ \(!i, !renBuf) (render, Position pos) -> do
                -- in future, we won't draw off screen objects
                let shouldDraw = True
                    
                renBuf' <- if i < MVec.length renBuf then pure renBuf
                           else lift . growRenderBuffer $ renBuf
                if shouldDraw then
                    (lift . MVec.unsafeWrite renBuf i) (convPos camera pos, render)
                    >> pure (i + 1, renBuf)
                else pure (i, renBuf)
        renderEvery :: Int -> Int -> RenderBuffer -> SDL.Surface -> IO ()
        renderEvery i len buf sur | i == len  = pure ()
                                  | otherwise = MVec.unsafeRead buf i >>= drawRender >> renderEvery (i + 1) len buf sur
            where drawRender (V3 x y _, Render sprs) = Vec.foldM_ (drawEach $ V2 x y) sur sprs
                  drawEach pos scr ren = drawRenderSprite (mkRenderRect resToCenter scale pos) ren scr

    (sprs, renBuf') <- cfoldM gatherRender (0, renBuf)
    if sprs > 0 then
        lift $ sortByBounds sortRenders renBuf' 0 (sprs) >> renderEvery 0 sprs renBuf' screen
    else pure ()

    SDL.updateWindowSurface window
    pure renBuf'
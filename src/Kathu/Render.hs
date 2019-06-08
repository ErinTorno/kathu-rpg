{-# LANGUAGE BangPatterns #-}

module Kathu.Render where

import Apecs hiding (($=))
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Word
import Foreign.C.Types (CInt)
import Kathu.Entity.Action
import Kathu.Entity.ActorState
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Graphics.Camera
import Kathu.Graphics.Color
import Kathu.Graphics.Drawable
import Kathu.Graphics.Drawing
import Kathu.Graphics.ImageManager
import Kathu.Graphics.RenderBuffer
import Kathu.Graphics.UI
import Kathu.IO.Settings
import Kathu.World.Field
import Kathu.World.Tile
import Kathu.World.WorldSpace
import qualified Kathu.Util.SDLCommon as SDLC
import Kathu.Util.Misc
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import qualified SDL

-- right now we stick with 45 degrees, as tiles are not stretched to accommodate other angles
cameraAngle :: Floating a => a
cameraAngle = 45.0

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
edgeBleedScaling = 1.005

logicCoordToRender :: Floating a => a -> V3 a -> V3 a -> V3 a
logicCoordToRender scale (V3 topX topY topZ) (V3 tarX tarY tarZ) = V3 x' y' z'
    where x' = (tarX - topX) * scale
          -- this ensures that the z angle is factored into where it appears
          y' = (tarY - topY + cameraZMult * (tarZ - topZ)) * scale
          z' = tarY - topY -- used only for sorting, closer along y coord is only consideration
          -- these help it render floors/tiles on other layers; it's just a patch until we get tiles seperated by flat or wall
          -- y' = z' * scale
          -- z' = (tarY - topY + cameraZMult * (tarZ - topZ))

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
    world    <- get global
    imageManager <- get global

    -- clears background
    let (Color bgColor) = backgroundColor imageManager
    SDL.surfaceFillRect screen Nothing bgColor

    zoom <- cfold (\z (Camera zoom) -> zoom) 1.0

    let scale   = (fromIntegral . resolutionY $ settings) / (zoom * pixelsPerUnit * unitsPerHeight)
        (V2 _ resY) = resolution settings
        resToCenter = ((*) 0.5 . fromIntegral) <$> resolution settings
        unitsPerHeight = minUnitsPerHeight + (maxUnitsPerHeight - minUnitsPerHeight) * pixMult
            where pixMult = fromIntegral (clampBetween pixelsForMinUnits pixelsForMaxUnits resY) / fromIntegral pixelsForMaxUnits
        unitsPerWidth = unitsPerHeight * aspectRatio resToCenter
        camShiftUp = 12
        -- this collects all renders into our buffer with their positions
        -- this takes transformed V3, rather than logical, since z is fully depth, rather than up down
        gatherRender :: (Int, RenderBuffer) -> (Camera, Position) -> SystemT' IO (Int, RenderBuffer)
        gatherRender (i, buf) (Camera _, Position cam@(V3 camX camY camZ)) =
            let cam'@(V3 camX' camY' _) = V3 camX (camY - camShiftUp) camZ
                convPos = logicCoordToRender scale cam'

                minY = camY' + ((-0.5) * zoom * unitsPerHeight) * pixelsPerUnit
                maxY = camY' + (0.5    * zoom * unitsPerHeight + renderBorderUnits) * pixelsPerUnit
                minX = camX' + ((-0.5) * zoom * unitsPerWidth  - renderBorderUnits) * pixelsPerUnit
                maxX = camX' + (0.5    * zoom * unitsPerWidth  + renderBorderUnits) * pixelsPerUnit

                isOffScreen (V3 !x !y !z) = let y' = y + cameraZMult * z in y' < minY || y' > maxY || x < minX || x > maxX
                -- adds to RenderBuffer and increments if judged to be drawable; expands the buffer if needed
                addToBuffer :: (Int, RenderBuffer) -> Vector RenderSprite -> V3 Float -> SystemT' IO (Int, RenderBuffer)
                addToBuffer (!i, !renBuf) render pos = if isOffScreen pos then pure (i, renBuf) else do
                    -- in future, we won't draw off screen objects
                    let renderPos = convPos pos
                        shouldDraw = True
                    renBuf' <- lift $ growMVecIfNeeded bufferGrowIncr i buf
                    if shouldDraw then
                        (lift . MVec.unsafeWrite renBuf i) (renderPos, render)
                        >> pure (i + 1, renBuf)
                    else pure (i, renBuf)

                -- this call to fieldFoldM is (as of when this is written) the most time intensive process in the program
                -- if we start to have too many problems, we should like into implementing caching each individual sprite into larger ones
                -- that represent each layer in a field
                -- although to preserve z-depth we might want to split each field into "strips" of tiles with same y and z positions
                -- close to this was "surfaceBlitScaled", which would dramatically have its number of calls cut down by this too
                foldFnField :: (Int, RenderBuffer) -> (V3 Int, Field) -> SystemT' IO (Int, RenderBuffer)
                foldFnField pair (!pos, !field) = fieldFoldM (addTile pos) pair field
                addTile :: V3 Int -> (Int, RenderBuffer) -> V3 Int -> TileState -> SystemT' IO (Int, RenderBuffer)
                addTile fpos pair pos t | t^.tile.tileID == emptyTileID = pure pair
                                        | otherwise = addToBuffer pair (t^.tile.tileRender.to unRender) (worldCoordFromTile fpos pos)

            in ((flip cfoldM) (i, buf) $ \pair (Render render, Position pos) -> addToBuffer pair render pos)
               >>= \acc -> foldM foldFnField acc (fieldsSurrounding cam world) 
        renderEvery :: Int -> Int -> RenderBuffer -> SDL.Surface -> IO ()
        renderEvery !i !len !buf !sur | i == len  = pure ()
                                      | otherwise = MVec.unsafeRead buf i >>= drawRender >> renderEvery (i + 1) len buf sur
            where drawRender (V3 x y _, sprs) = Vec.foldM_ (drawEach $ V2 x y) sur sprs
                  drawEach pos scr ren = blitRenderSprite imageManager (mkRenderRect edgeBleedScaling resToCenter scale pos) ren scr

    (sprCount, renBuf') <- cfoldM gatherRender (0, renBuf)
    if sprCount > 0 then
        lift $ sortRenderBuffer 0 sprCount renBuf' >> renderEvery 0 sprCount renBuf' screen
    else pure ()

    playerAS <- cfold (\acc (as, Camera _) -> Just as) Nothing
    renderUI screen scale playerAS

    SDL.updateWindowSurface window
    pure renBuf'
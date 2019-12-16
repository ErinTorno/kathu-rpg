{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Kathu.App.Graphics.Render where

import Apecs hiding (($=))
import Apecs.Physics hiding (($=))
import Control.Lens hiding (Identity)
import Control.Monad (foldM, when)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Word
import Linear.V2 (V2(..), _y)
import SDL (($=))
import qualified SDL

import Kathu.App.Data.Settings
import Kathu.App.Graphics.Debug (renderDebug)
import Kathu.App.Graphics.Drawing
import Kathu.App.Graphics.Image (ImageID)
import Kathu.App.Graphics.ImageManager
import Kathu.App.Graphics.RenderBuffer
import Kathu.App.Graphics.UI
import Kathu.App.System (SystemT')
import Kathu.Entity.Action
import Kathu.Entity.System
import Kathu.Graphics.Camera
import Kathu.Graphics.Color (unColor)
import Kathu.Graphics.Drawable
import Kathu.World.Field
import Kathu.World.Tile hiding (Vector, MVector)
import Kathu.World.WorldSpace
import Kathu.Util.Collection (growMVecIfNeeded, mapMVec)
import Kathu.Util.Numeric (clampBetween)

-- the height of the screen in units; depending on screen size, more or less is included

minUnitsPerHeight :: (Floating a, RealFrac a) => a
minUnitsPerHeight = 8.0

maxUnitsPerHeight :: (Floating a, RealFrac a) => a
maxUnitsPerHeight = 14.0

pixelsForMinUnits :: Integral a => a
pixelsForMinUnits = 360

pixelsForMaxUnits :: Integral a => a
pixelsForMaxUnits = 1080

-- if sprite position is more than this many units from left or right, or from bottom, we don't draw
-- we don't draw anything above the top of the screen, however, since sprites draw out and upwards
renderBorderUnits :: (Floating a, RealFrac a) => a
renderBorderUnits = 3.0

pixelsPerUnit :: (Floating a, RealFrac a) => a
pixelsPerUnit = 16.0

aspectRatio :: (Floating a, RealFrac a) => V2 a -> a
aspectRatio (V2 x y) = x / y

-- sprite dimensions are multiplied by this to prevent tiny streaks between adjacent sprites
edgeBleedScaling :: (Floating a, RealFrac a) => a
edgeBleedScaling = 1.005

logicCoordToRender :: (Floating a, RealFrac a) => a -> V2 a -> V2 a -> V2 a
logicCoordToRender scale (V2 topX topY) (V2 tarX tarY) = V2 ((tarX - topX) * scale) ((tarY - topY) * scale)

updateAnimations :: Word32 -> SystemT' IO ()
updateAnimations dT = do
    -- anim frames are updated
    let updateFramesIfAnim (RSAnimated !anim) = RSAnimated $ updateFrames dT anim
        updateFramesIfAnim s                  = s
        updateWithoutController :: (Render ImageID, Not ActionSet) -> Render ImageID
        updateWithoutController (Render sprites, _) = Render $ updateFramesIfAnim <$> sprites
        updateAnimated :: (Render ImageID, ActionSet) -> Render ImageID
        updateAnimated ((Render sprites), ActionSet {_moving = m, _lastMoving = lm, _facingDirection = fac}) = Render $ updateEach <$> sprites
            where updateEach s@(RSStatic _)     = s
                  updateEach (RSAnimated anim)  = RSAnimated $ update m lm fac anim
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

    (Tiles tileVector) <- get global :: SystemT' IO (Tiles ImageID)
    lift . mapMVec tileVector $ (over tileRender $ Render . fmap updateFramesIfAnim . unRender)

----------------------
-- main render loop --
----------------------

runRender :: SDL.Renderer -> RenderBuffer -> Word32 -> SystemT' IO RenderBuffer
runRender !renderer !renderBuffer !dT = do
    do stepRenderTime dT
    updateAnimations dT

    runImageManager
    imageManager   <- get global
    settings       <- get global
    (Debug isDebug)    <- get global
    (Tiles tileVector) <- get global
    let getTile :: TileState -> SystemT' IO (Tile ImageID)
        getTile = lift . MVec.read tileVector . fromIntegral .  unTileID . view tile

    world :: WorldSpace ImageID <- get global
    (camPos@(V2 camX camY), zoomScale) <- cfold (\_ (Position pos, Camera z) -> (pos, z)) (V2 0 0, 1.0)

    -- clears background
    SDL.rendererDrawColor renderer $= (unColor . backgroundColor $ imageManager)
    SDL.clear renderer

    let scale      = (fromIntegral . view _y . resolution $ settings) / (zoomScale * unitsPerHeight * pixelsPerUnit)
        logicScale = scale * pixelsPerUnit -- we mult by this again to convert the 1-per-tile view of the entity-world into a N-pixels-per-tile view
        (V2 _ resY) = resolution settings
        resToCenter = ((*) 0.5 . fromIntegral) <$> resolution settings
        unitsPerHeight = minUnitsPerHeight + (maxUnitsPerHeight - minUnitsPerHeight) * pixMult
            where pixMult = fromIntegral (clampBetween pixelsForMinUnits pixelsForMaxUnits resY) / fromIntegral (pixelsForMaxUnits :: Int)
        unitsPerWidth = unitsPerHeight * aspectRatio resToCenter
        camShiftUp = 0.75

        shiftedCamera@(V2 camX' camY')  = V2 camX (camY - camShiftUp)
        isOffScreen (V2 !x !y) = y < minY || y > maxY || x < minX || x > maxX
        -- coordinates must be in these ranges for us to draw them
        -- if a sprite is taller or wider than units * 16px, then it can potentially be on screen but not drawn
        -- this is not a problem now, but it it occurs later, this can easily be fixed
        minY = camY' + ((-0.5) * zoomScale * unitsPerHeight) -- images are drawn from their bottom center, so if y is above top, no need to draw
        maxY = camY' + (0.5    * zoomScale * unitsPerHeight + renderBorderUnits * 2) -- like above, but we need extra room for tall objects below
        minX = camX' + ((-0.5) * zoomScale * unitsPerWidth  - renderBorderUnits)
        maxX = camX' + (0.5    * zoomScale * unitsPerWidth  + renderBorderUnits)

        -- adds to RenderBuffer and increments if judged to be drawable; expands the buffer if needed
        addToBuffer :: (Int, RenderBuffer) -> Vector (RenderSprite ImageID) -> V2 Double -> SystemT' IO (Int, RenderBuffer)
        addToBuffer (!idx, !renBuf) render pos = if isOffScreen pos then pure (idx, renBuf) else do
            let renderPos = (*(logicScale)) <$> (pos - shiftedCamera)
            renBuf' <- lift $ growMVecIfNeeded renBuf bufferGrowIncr idx
            (lift . MVec.unsafeWrite renBuf' idx) (renderPos, render)
                >> pure (idx + 1, renBuf')

        -- this call to fieldFoldM is (as of when this is written) the most time intensive process in the program
        -- if we start to have too many problems, we should like into implementing caching each individual sprite into larger ones
        -- although to preserve z-depth we might want to split each field into "strips" of tiles with same y and z positions
        -- close to this was "surfaceBlitScaled", which would dramatically have its number of calls cut down by this too
        foldFnField :: (Int, RenderBuffer) -> (V2 Int, Field) -> SystemT' IO (Int, RenderBuffer)
        foldFnField pair (!pos, !field) = fieldFoldM (addTile pos) pair field
        addTile :: V2 Int -> (Int, RenderBuffer) -> V2 Int -> TileState -> SystemT' IO (Int, RenderBuffer)
        addTile fpos pair pos !t = getTile t >>= \tileInst ->
            -- foldFnField filters out empty tiles, so we know they are safe here; if it didn't, attempting to render an empty would error
            addToBuffer pair (getTileRenderSprites t tileInst) (worldCoordFromTileCoord fpos pos)

        gatherEntityRender :: (Int, RenderBuffer) -> (Render ImageID, Position) -> SystemT' IO (Int, RenderBuffer)
        gatherEntityRender pair (Render render, Position pos) = addToBuffer pair render pos

        gatherTileRender :: (Int, RenderBuffer) -> SystemT' IO (Int, RenderBuffer)
        gatherTileRender pair = foldM foldFnField pair . fieldsSurrounding camPos $ world 

        -- this collects all renders into our buffer with their positions
        gatherRender :: (Int, RenderBuffer) -> SystemT' IO (Int, RenderBuffer)
        gatherRender pair = cfoldM gatherEntityRender pair >>= gatherTileRender
        
        renderEvery :: Int -> Int -> RenderBuffer -> IO ()
        renderEvery !i !len !buf | i == len  = pure ()
                                 | otherwise = MVec.unsafeRead buf i >>= drawRender >> renderEvery (i + 1) len buf
            where drawRender (V2 x y, sprs) = Vec.forM_ sprs (drawEach $ V2 x y)
                  drawEach pos ren      = blitRenderSprite renderer imageManager (mkRenderRect edgeBleedScaling resToCenter scale pos) ren

    (sprCount, renBuf') <- gatherRender (0, renderBuffer)
    when (sprCount > 0) $
        lift (sortRenderBuffer 0 sprCount renBuf' >> renderEvery 0 sprCount renBuf')

    playerAS <- cfold (\_ (as, Camera _) -> Just as) Nothing
    renderUI renderer scale playerAS

    when isDebug $
        renderDebug renderer (\pos -> ((+)resToCenter) . fmap (*logicScale) . (+(pos - shiftedCamera)))

    SDL.present renderer
    pure renderBuffer
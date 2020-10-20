module Kathu.App.Graphics.Render where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   hiding (($=))
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (foldM, when)
import           Data.Maybe                      (fromMaybe)
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as Vec
import qualified Data.Vector.Mutable             as MVec
import           Data.Word
import           SDL                             (($=))
import qualified SDL

import           Kathu.App.Data.Settings
import           Kathu.App.Graphics.Debug        (renderDebug)
import           Kathu.App.Graphics.Drawing
import           Kathu.App.Graphics.Image        (ImageID)
import           Kathu.App.Graphics.ImageManager
import           Kathu.App.Graphics.RenderBuffer
import           Kathu.App.Graphics.UI
import           Kathu.App.System                (SystemT')
import           Kathu.App.Tools.ToolSystem      (renderToolMode)
import           Kathu.Entity.Action
import           Kathu.Entity.System
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Color (unColor)
import           Kathu.Graphics.Drawable
import           Kathu.World.Field
import           Kathu.World.Tile                hiding (Vector, MVector)
import           Kathu.World.WorldSpace
import           Verda.Util.Containers           (forMVec)
import           Verda.Util.Apecs

-- if sprite position is more than this many units from left or right, or from bottom, we don't draw
-- we don't draw anything above the top of the screen, however, since sprites draw out and upwards
renderBorderUnits :: Floating a => a
renderBorderUnits = 3.0

logicCoordToRender :: Floating a => a -> V2 a -> V2 a -> V2 a
logicCoordToRender scale (V2 topX topY) (V2 tarX tarY) = V2 ((tarX - topX) * scale) ((tarY - topY) * scale)

updateAnimations :: Word32 -> SystemT' IO ()
updateAnimations dT = do
    -- anim frames are updated
    let updateFramesIfAnim (RSAnimated !anim) = RSAnimated $ updateFrames dT anim
        updateFramesIfAnim s                  = s
        -- just advances to next frame, no special animation-switching is needed
        updateWithoutController :: (Render ImageID, Not ActionSet) -> Render ImageID
        updateWithoutController (Render sprites, _) = Render $ updateFramesIfAnim <$> sprites
        -- since this has actions, we also need to check and see if it should change animation states
        updateAnimated :: (Render ImageID, ActionSet) -> Render ImageID
        updateAnimated (Render sprites, ActionSet {_moving = m, _facingDirection = fac}) = Render $ updateEach <$> sprites
            where updateEach (RSAnimated anim)  = RSAnimated $ update m anim
                  updateEach s                  = s
                  update Nothing anim =
                      -- we ensure that this is paused and waiting on first frame
                      anim {activeAnim = dirToAnimIndex fac, currentFrame = 0, animTime = timeBeforeFrameChange anim}
                  update (Just d) anim
                      | dir == act  = updateFrames dT anim -- if same direction, we just update its frames
                      | otherwise   = switchAnimation dir anim -- we switch to new animation and reset
                          where dir = dirToAnimIndex d
                                act = activeAnim anim
    -- only update frames for those without any controller for them
    cmap updateWithoutController
    -- if it has an ActionSet, we have to deal with swapping animations
    cmap updateAnimated

    -- since tile graphics information isn't stored as entities, we instead just grab all tiles and update their animations
    Tiles tileVector <- get global :: SystemT' IO (Tiles ImageID)
    lift . forMVec tileVector $ over tileRender (\(Render frames) -> Render (updateFramesIfAnim <$> frames))

----------------------
-- main render loop --
----------------------

runRender :: SDL.Renderer -> RenderBuffer -> Word32 -> SystemT' IO ()
runRender !renderer !renderBuffer !dT = do
    stepRenderTime dT
    updateAnimations dT

    runImageManager
    imageManager     <- get global
    settings         <- get global
    Debug isDebug    <- get global
    Tiles tileVector <- get global
    let getTile :: TileState -> SystemT' IO (Tile ImageID)
        getTile = lift . MVec.read tileVector . fromIntegral .  unTileID . view tile

    world :: WorldSpace ImageID <- get global
    (Position (V2 camX camY), Camera zoomScale) <- fromMaybe (Position (V2 0 0), Camera 1) <$> getUnique

    -- clears background
    SDL.rendererDrawColor renderer $= (unColor . backgroundColor $ imageManager)
    SDL.clear renderer

    let V2 _ resY      = resolution settings
        res            = fromIntegral <$> resolution settings
        unitsPerHeight = getUnitsPerHeight resY
        unitsPerWidth  = unitsPerHeight * aspectRatio res

        scale          = getScale (fromIntegral resY) unitsPerHeight zoomScale
        worldToScreen  = worldToScreenScale res scale camX camY 
        V2 camX' camY' = V2 camX (camY - cameraShiftUp)
        isOffScreen (V2 !x !y) = y < minY || y > maxY || x < minX || x > maxX
        -- coordinates must be in these ranges for us to draw them
        -- if a sprite is taller or wider than units * 16px, then it can potentially be on screen but not drawn
        -- this is not a problem now, but it it occurs later, this can easily be fixed
        minY = camY' + ((-0.5) * zoomScale * unitsPerHeight) -- images are drawn from their bottom center, so if y is above top, no need to draw
        maxY = camY' + (0.5    * zoomScale * unitsPerHeight + renderBorderUnits * 2) -- like above, but we need extra room for tall objects below
        minX = camX' + ((-0.5) * zoomScale * unitsPerWidth  - renderBorderUnits)
        maxX = camX' + (0.5    * zoomScale * unitsPerWidth  + renderBorderUnits)

        -- adds to RenderBuffer and increments if judged to be drawable; expands the buffer if needed
        addToBuffer :: Int -> Vector (RenderSprite ImageID) -> V2 Double -> SystemT' IO Int
        addToBuffer !idx render !pos
            | isOffScreen pos = pure idx
            | otherwise       = lift $ do
                let renderPos = worldToScreen pos
                Vec.ifoldM'_ (\_ i spr -> writeToBuffer (idx + i) (renderPos, spr) renderBuffer) () render
                pure $ idx + Vec.length render

        -- this call to fieldFoldM is (as of when this is written) the most time intensive process in the program
        -- if we start to have too many problems, we should like into implementing caching each individual sprite into larger ones
        -- although to preserve z-depth we might want to split each field into "strips" of tiles with same y and z positions
        -- close to this was "surfaceBlitScaled", which would dramatically have its number of calls cut down by this too
        foldFnField :: Int -> (V2 Int, Field) -> SystemT' IO Int
        foldFnField !idx (!pos, !field) = fieldFoldM (addTile pos) idx field
        addTile :: V2 Int -> Int -> V2 Int -> TileState -> SystemT' IO Int
        addTile fpos i pos !t = getTile t >>= \tileInst ->
            -- foldFnField filters out empty tiles, so we know they are safe here; if it didn't, attempting to render an empty would error
            let (Render layers) = getTileRender t tileInst
             in addToBuffer i layers (worldCoordFromTileCoord fpos pos)

        gatherEntityRender :: Int -> (Render ImageID, Position) -> SystemT' IO Int
        gatherEntityRender i (Render render, Position pos) = addToBuffer i render pos

        gatherTileRender :: Int -> SystemT' IO Int
        gatherTileRender i = foldM foldFnField i . fieldsSurrounding camX camY $ world 

        -- this collects all renders into our buffer with their positions
        gatherRender :: SystemT' IO Int
        gatherRender = cfoldM gatherEntityRender 0 >>= gatherTileRender
        
        renderEvery :: Int -> Int -> IO ()
        renderEvery !i !len
            | i == len    = pure ()
            | otherwise   = do
                let drawEach !pos !ren = blitRenderSprite renderer imageManager (mkRenderRect edgeBleedScaling scale pos) ren
                (!pos, !spr) <- readFromBuffer i renderBuffer
                drawEach pos spr
                renderEvery (i + 1) len

    sprCount <- gatherRender
    when (sprCount > 0) $
        lift (sortRenderBuffer 0 sprCount renderBuffer >> renderEvery 0 sprCount)

    playerAS <- cfold (\_ (as, Camera _) -> Just as) Nothing
    renderUI renderer scale playerAS

    renderToolMode renderer worldToScreen

    when isDebug $
        renderDebug renderer worldToScreen

    SDL.present renderer
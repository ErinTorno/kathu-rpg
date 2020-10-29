module Kathu.App.Graphics.Render where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   hiding (($=))
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (foldM, when)
import           Data.Maybe                      (fromMaybe)
import           Control.Monad.IO.Class          (MonadIO)
import qualified Data.Vector                     as Vec
import qualified Data.Vector.Mutable             as MVec
import           Data.Word
import           SDL                             (($=))
import qualified SDL
import           Verda.Graphics.Color            (unColor, white)
import           Verda.Graphics.Components
import           Verda.Graphics.Drawing
import           Verda.Graphics.SpriteBuffer
import           Verda.Graphics.Sprites
import           Verda.Util.Containers           (forMVec)
import           Verda.Util.Apecs

import           Kathu.App.System                (SystemT')
import           Kathu.App.Tools.ToolSystem      (renderToolMode)
import           Kathu.Entity.Action
import           Kathu.Entity.System
import           Kathu.World.Field
import           Kathu.World.Tile                hiding (Vector, MVector)
import           Kathu.World.WorldSpace

-- if sprite position is more than this many units from left or right, or from bottom, we don't draw
-- we don't draw anything above the top of the screen, however, since sprites draw out and upwards
renderBorderUnits :: Floating a => a
renderBorderUnits = 3.0

logicCoordToRender :: Floating a => a -> V2 a -> V2 a -> V2 a
logicCoordToRender scale (V2 topX topY) (V2 tarX tarY) = V2 ((tarX - topX) * scale) ((tarY - topY) * scale)

updateAnimations :: Word32 -> SystemT' IO ()
updateAnimations dT = do
    let updateAnimated (sprite, ActionSet {_moving = mv, _facingDirection = fac}) = case mv of
            Nothing -> setAnimationID (dirToAnimIndex fac) sprite
            Just dir -> if
                | dirIdx == getAnimationID sprite -> updateFrames dT sprite
                | otherwise                       -> setAnimationID dirIdx sprite
                where dirIdx = dirToAnimIndex dir

    cmap (\(sprite, _ :: Not ActionSet) -> updateFrames dT sprite)
    cmap updateAnimated
    -- since tile graphics information isn't stored as entities, we instead just grab all tiles and update their animations
    Tiles tileVector <- get global :: SystemT' IO Tiles
    lift $ forMVec tileVector (updateTileAnimation dT)

----------------------
-- main render loop --
----------------------

runRender :: SDL.Renderer -> SpriteBuffer -> Word32 -> SystemT' IO ()
runRender !renderer !spriteBuffer !dT = do
    stepRenderTime dT
    updateAnimations dT

    Resolution resolution@(V2 _ resY) <- get global
    spriteManager    <- get global
    Tiles tileVector <- get global
    let getTile :: TileState -> SystemT' IO Tile
        getTile = lift . MVec.read tileVector . fromIntegral .  unTileID . view tile

    world :: WorldSpace <- get global
    (Position camPos@(V2 camX camY), Camera zoomScale) <- fromMaybe (Position (V2 0 0), Camera 1) <$> getUnique

    -- clears background
    BackgroundColor bkgColor <- get global
    SDL.rendererDrawColor renderer $= unColor bkgColor
    SDL.clear renderer

    let res            = fromIntegral <$> resolution
        unitsPerHeight = getUnitsPerHeight resY
        unitsPerWidth  = unitsPerHeight * getAspectRatio res

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

        -- adds to spriteBuffer and increments if judged to be drawable; expands the buffer if needed
        addToBuffer :: MonadIO m => Int -> Sprite -> V2 Double -> Tint -> m Int
        addToBuffer !idx !sprite !pos (Tint tint)
            | isOffScreen pos = pure idx
            | otherwise       = do
                let renderPos = worldToScreen pos
                liftIO $ sbeWrite spriteBuffer idx (SpriteBufferElement renderPos tint sprite)
                pure $ idx + 1

        -- this call to fieldFoldM is (as of when this is written) the most time intensive process in the program
        -- if we start to have too many problems, we should like into implementing caching each individual sprite into larger ones
        -- although to preserve z-depth we might want to split each field into "strips" of tiles with same y and z positions
        -- close to this was "surfaceBlitScaled", which would dramatically have its number of calls cut down by this too
        foldFnField :: Int -> (V2 Int, Field) -> SystemT' IO Int
        foldFnField !idx (!pos, !field) = fieldFoldM (addTile pos) idx field
        addTile :: V2 Int -> Int -> V2 Int -> TileState -> SystemT' IO Int
        addTile fpos i pos !t = getTile t >>= \tileInst ->
            -- foldFnField filters out empty tiles, so we know they are safe here; if it didn't, attempting to render an empty would error
            addToBuffer i (getTileSprite t tileInst) (worldCoordFromTileCoord fpos pos) (Tint white)

        gatherEntitySprite i (Position pos, sprite, t :: Maybe Tint) = addToBuffer i sprite pos (fromMaybe (Tint white) t)

        gatherTileSprite :: Int -> SystemT' IO Int
        gatherTileSprite !idx = foldM foldFnField idx . fieldsSurrounding camX camY $ world
        
        renderEvery :: Int -> Int -> IO ()
        renderEvery !idx !len
            | idx == len = pure ()
            | otherwise  = do
                SpriteBufferElement !pos !color !sprite <- sbeRead spriteBuffer idx
                let mkDestRect = mkRenderRect edgeBleedScaling (scale * sprite^.spriteScale) pos
                blitSprite renderer spriteManager sprite color mkDestRect
                renderEvery (idx + 1) len

        runExtensions :: Vec.Vector SpriteRenderExtension -> Int -> IO Int
        runExtensions exts idx = Vec.foldM' (\acc (SpriteRenderExtension ext) -> ext addToBuffer camPos (V2 unitsPerWidth unitsPerHeight) acc) idx exts

    RenderExtensions spriteExts renExts <- get global
    sprCount <- cfoldM gatherEntitySprite 0
            >>= gatherTileSprite
            >>= liftIO . runExtensions spriteExts
    
    when (sprCount > 0) $
        lift (sortSpriteBuffer spriteBuffer 0 sprCount >> renderEvery 0 sprCount)

    liftIO $ Vec.mapM_ (\(RendererExtension ext) -> ext renderer worldToScreen camPos) renExts

    renderToolMode renderer worldToScreen

    SDL.present renderer
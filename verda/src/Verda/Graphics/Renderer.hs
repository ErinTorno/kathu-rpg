module Verda.Graphics.Renderer where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   hiding (($=))
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (when)
import           Data.Maybe                      (fromMaybe)
import           Control.Monad.IO.Class          (MonadIO)
import qualified Data.Vector                     as Vec
import           Data.Word
import           SDL                             (($=))
import qualified SDL
import           Verda.Graphics.Color            (unColor, white)
import           Verda.Graphics.Components
import           Verda.Graphics.Drawing
import           Verda.Graphics.SpriteBuffer
import           Verda.Graphics.Sprites
import           Verda.Time                      (stepRenderTime)
import           Verda.Util.Apecs
import           Verda.World

-- if sprite position is more than this many units from left or right, or from bottom, we don't draw
-- we don't draw anything above the top of the screen, however, since sprites draw out and upwards
renderBorderUnits :: Floating a => a
renderBorderUnits = 3.0

----------------------
-- main render loop --
----------------------

runRender :: VerdaWorld w IO => SDL.Renderer -> SpriteBuffer -> Word32 -> SystemT w IO ()
runRender !renderer !spriteBuffer !dT = do
    stepRenderTime dT
    RenderExtensions spriteExts renExts beforeExts <- get global
    cmap $ \sprite -> updateFrames dT sprite
    liftIO $ mapM_ (\(BeginRenderExtension ext) -> ext dT renderer) beforeExts

    Resolution resolution@(V2 _ resY) <- get global
    spriteManager    <- get global

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

        gatherEntitySprite i (Position pos, sprite, t :: Maybe Tint) = addToBuffer i sprite pos (fromMaybe (Tint white) t)
        
        renderEvery :: Int -> Int -> IO ()
        renderEvery !idx !len
            | idx == len = pure ()
            | otherwise  = do
                SpriteBufferElement !pos !color !sprite <- sbeRead spriteBuffer idx
                let mkDestRect = mkRenderRect edgeBleedScaling (scale * sprite^.spriteScale) pos
                blitSprite renderer spriteManager sprite color mkDestRect
                renderEvery (idx + 1) len

        runExtensions :: Vec.Vector SpriteRenderExtension -> Int -> IO Int
        runExtensions exts idx = Vec.foldM' (\acc (SpriteRenderExtension ext) -> ext dT addToBuffer camPos (V2 unitsPerWidth unitsPerHeight) acc) idx exts

    sprCount <- cfoldM gatherEntitySprite 0
            >>= liftIO . runExtensions spriteExts
    
    when (sprCount > 0) $
        lift (sortSpriteBuffer spriteBuffer 0 sprCount >> renderEvery 0 sprCount)

    liftIO $ Vec.mapM_ (\(RendererExtension ext) -> ext renderer worldToScreen camPos) renExts

    SDL.present renderer
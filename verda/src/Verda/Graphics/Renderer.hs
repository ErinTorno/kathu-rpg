module Verda.Graphics.Renderer (runRender) where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   hiding (($=))
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (when)
import           Data.Maybe                      (fromMaybe)
import           Control.Monad.IO.Class          (MonadIO)
import qualified Data.Vector                     as Vec
import           Data.Word
import qualified Graphics.Rendering.OpenGL       as GL
import           SDL                             (($=))
import qualified SDL
import           Verda.Graphics.Color            (unColor, white)
import           Verda.Graphics.Components
import           Verda.Graphics.Drawing
import           Verda.Graphics.Shaders          (getProgram)
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

runRender :: VerdaWorld w IO => SDL.Window -> SDL.Renderer -> SDL.Texture -> SpriteBuffer -> Word32 -> SystemT w IO ()
runRender !window !renderer !screenTex !spriteBuffer !dT = do
    stepRenderTime dT
    RenderExtensions spriteExts renExts beforeExts <- get global
    cmap $ \sprite -> updateFrames dT sprite
    liftIO $ mapM_ (\(BeginRenderExtension ext) -> ext dT renderer) beforeExts

    Resolution resolution@(V2 _ resY) <- get global
    spriteManager    <- get global

    (Position camPos@(V2 camX camY), Camera zoomScale) <- fromMaybe (Position (V2 0 0), Camera 1) <$> getUnique

    -- clears background
    BackgroundColor bkgColor <- get global
    SDL.rendererRenderTarget renderer $= Just screenTex
    SDL.rendererDrawColor renderer $= unColor bkgColor
    SDL.clear renderer

    let resFl          = fromIntegral <$> resolution
        unitsPerHeight = getUnitsPerHeight resY
        unitsPerWidth  = unitsPerHeight * getAspectRatio resFl

        scale          = getScale (fromIntegral resY) unitsPerHeight zoomScale
        worldToScreen  = worldToScreenScale resFl scale camX camY 
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
    -- if any RendererExtension uses primitive draw ops, a copy needs to be performed or else the screen whites out
    -- no clue why
    when (Vec.length renExts > 0) $
        let smallRect = Just (SDL.Rectangle (SDL.P 0) 16)
         in SDL.copy renderer screenTex smallRect smallRect

    shaders <- get global
    liftIO $ renderGLVertex window renderer screenTex shaders resolution

-- | Bind and draw screen tex on square in world
renderGLVertex :: SDL.Window -> SDL.Renderer -> SDL.Texture -> ShaderSet -> V2 Int -> IO ()
renderGLVertex window renderer screenTex shaders res = do
    SDL.rendererRenderTarget renderer $= Nothing
    SDL.glBindTexture screenTex
    
    prevProg <- SDL.get GL.currentProgram
    GL.currentProgram SDL.$= getProgram shaders

    let minX, maxX, minY, maxY, minU, maxU, minV, maxV :: GL.GLfloat
        V2 resGLx resGLy = fromIntegral <$> res
        minX = 0
        minY = 0
        maxX = resGLx
        maxY = resGLy
        minU = 0
        minV = 1 -- swap with maxV to flip vertical
        maxU = 1
        maxV = 0
    GL.renderPrimitive GL.TriangleStrip $ do
        GL.color    $ GL.Color3 1 1 (1 :: GL.GLfloat)
        GL.texCoord $ GL.TexCoord2 minU minV
        GL.vertex   $ GL.Vertex2   minX minY
        GL.texCoord $ GL.TexCoord2 maxU minV
        GL.vertex   $ GL.Vertex2   maxX minY
        GL.texCoord $ GL.TexCoord2 minU maxV
        GL.vertex   $ GL.Vertex2   minX maxY
        GL.texCoord $ GL.TexCoord2 maxU maxV
        GL.vertex   $ GL.Vertex2   maxX maxY

    SDL.glSwapWindow window
    SDL.glUnbindTexture screenTex
    GL.currentProgram SDL.$= prevProg
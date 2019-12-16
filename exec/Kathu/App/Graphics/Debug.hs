{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.App.Graphics.Debug (renderDebug) where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   hiding (($=))
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (void, when)
import qualified Data.Text                       as T
import qualified Data.Vector.Storable            as SVec
import           SDL                             (($=))
import qualified SDL
import qualified SDL.Font                        as SDLF

import           Kathu.App.Data.Library          (font)
import           Kathu.App.Graphics.Image        (ImageID)
import           Kathu.App.Graphics.ImageManager (currentPalette)
import           Kathu.App.System                (SystemT')
import           Kathu.Entity.Components         (Identity(..))
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Color
import           Kathu.World.WorldSpace
import           Kathu.Util.Types                (unID)

primaryCollisionBoxColor :: Color
primaryCollisionBoxColor = mkColor 250 50 90 225

attachedCollisionBoxColor :: Color
attachedCollisionBoxColor = mkColor 45 170 225 225 -- mkColor 100 214 110 225

renderDebug :: SDL.Renderer -> (V2 Double -> V2 Double -> V2 Double) -> SystemT' IO ()
renderDebug renderer logicToRenderPos = do
    let renderCollision _ _ []             = pure ()
        renderCollision col pos vecs@(v:_) = do
            SDL.rendererDrawColor renderer $= unColor col
            let points = SVec.fromList (fmap floor . logicToRenderPos pos <$> snoc vecs v)
            -- we render in groups of 4 pixels to have thicker lines
            SDL.drawLines renderer $ SVec.map (SDL.P . (+(V2 0 0))) points
            SDL.drawLines renderer $ SVec.map (SDL.P . (+(V2 0 1))) points
            SDL.drawLines renderer $ SVec.map (SDL.P . (+(V2 1 0))) points
            SDL.drawLines renderer $ SVec.map (SDL.P . (+(V2 1 1))) points

    -- attached boxes don't have an identity or a position, as they exist on a separate entity with just a shape
    cmapM_ $ \(Shape parentEty (Convex vecs _), _ :: Not Identity)   -> do
        hasPos <- exists parentEty (Proxy :: Proxy Position)
        when hasPos $
            (\(Position pos) -> renderCollision attachedCollisionBoxColor pos vecs) =<< get parentEty
    cmapM_ $ \(Position pos, Shape _ (Convex vecs _), _ :: Identity) -> renderCollision primaryCollisionBoxColor pos vecs
    renderDebugText renderer

-- This method of rendering text takes up a ridiculous amount of CPU
-- Current when this is being called, CPU more than doubles
-- This'll need to be moved over to a cached-based system, especially if we want to have other text
renderDebugText :: SDL.Renderer -> SystemT' IO ()
renderDebugText renderer = do
    world :: WorldSpace ImageID <- get global
    manager <- get global
    ((V2 camX camY), cZoom) <- cfold (\_ (Position pos, Camera z) -> (pos, z)) ((V2 0 0), 1)

    let displayText = T.pack . concat $ ["palette: ", show (currentPalette manager), " zoom: ", show (1 / cZoom), " | world: ", show (unID . worldID $ world), " x: ", show camX, " y: ", show camY]

    library <- get global
    textSurface <- SDLF.solid (library^.font) (unColor white) displayText
    textTexture <- SDL.createTextureFromSurface renderer textSurface
    textSize    <- SDL.surfaceDimensions textSurface

    void $ SDL.copy renderer textTexture Nothing (Just $ SDL.Rectangle (SDL.P (V2 0 0)) textSize)

    SDL.freeSurface textSurface
    SDL.destroyTexture textTexture
    
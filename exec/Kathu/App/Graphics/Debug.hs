module Kathu.App.Graphics.Debug (renderDebug) where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   hiding (($=))
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (void, when)
import qualified Data.Text                       as T
import qualified Data.Vector.Storable            as SVec
import           SDL                             (($=))
import qualified SDL

import           Kathu.App.Graphics.Font         (defaultFont, renderText)
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
attachedCollisionBoxColor = mkColor 45 170 225 225

renderDebug :: SDL.Renderer -> (V2 Double -> V2 Double) -> SystemT' IO ()
renderDebug renderer logicToRenderPos = do
    let renderCollision _ _ []             = pure ()
        renderCollision col pos vecs@(v:_) = do
            SDL.rendererDrawColor renderer $= unColor col
            let convPos vpoint = logicToRenderPos (pos + vpoint)
                points = SVec.fromList (fmap floor . convPos <$> snoc vecs v)
            -- we render in groups of 4 pixels to have thicker lines
            SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 0 0)) points
            SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 0 1)) points
            SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 1 0)) points
            SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 1 1)) points

    -- attached boxes don't have an identity or a position, as they exist on a separate entity with just a shape
    cmapM_ $ \(Shape parentEty (Convex vecs _), _ :: Not Identity)   -> do
        hasPos <- exists parentEty (Proxy :: Proxy Position)
        when hasPos $
            (\(Position pos) -> renderCollision attachedCollisionBoxColor pos vecs) =<< get parentEty
    cmapM_ $ \(Position pos, Shape _ (Convex vecs _), _ :: Identity) -> renderCollision primaryCollisionBoxColor pos vecs
    renderDebugText renderer

renderDebugText :: SDL.Renderer -> SystemT' IO ()
renderDebugText renderer = do
    world :: WorldSpace ImageID <- get global
    manager <- get global
    (V2 camX camY, cZoom) <- cfold (\_ (Position pos, Camera z) -> (pos, z)) (V2 0 0, 1)

    let displayText = T.concat
            [ "palette: ",  T.pack . show . currentPalette $ manager
            , " zoom: ",    T.pack . show $ 1 / cZoom
            , " | world: ", world^.worldID.to unID
            , " x: ",       T.pack . show $ camX
            , " y: ",       T.pack . show $ camY
            ]

    fontCache <- get global
    void $ renderText renderer fontCache defaultFont white (V2 0 0) displayText
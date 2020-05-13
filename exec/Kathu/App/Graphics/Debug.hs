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
import           Kathu.App.Tools.ToolMode        (isNoTool)
import           Kathu.Entity.Physics.CollisionGroup (collisionFilterDebugColor)
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Color
import           Kathu.World.WorldSpace
import           Kathu.Util.Types                (unID)

renderDebug :: SDL.Renderer -> (V2 Double -> V2 Double) -> SystemT' IO ()
renderDebug renderer logicToRenderPos = do
    -- in tool modes collision might be rapidly changing or out-of-date; don't bother to draw
    shouldDrawCol <- isNoTool <$> get global
    when shouldDrawCol $ do
        let renderCollision _ _ []                = pure ()
            renderCollision colFil pos vecs@(v:_) = do
                let convPos vpoint = logicToRenderPos (pos + vpoint)
                    points = SVec.fromList (fmap floor . convPos <$> snoc vecs v)

                SDL.rendererDrawColor renderer $= unColor (collisionFilterDebugColor colFil)
                -- we render in groups of 4 pixels to have thicker lines
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 0 0)) points
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 0 1)) points
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 1 0)) points
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 1 1)) points

        cmapM_ $ \(Shape _ (Convex vecs _), colFil :: CollisionFilter, Position pos) ->
            renderCollision colFil pos vecs
        -- attached boxes don't have a position, as they exist on a separate entity with just a shape
        cmapM_ $ \(Shape parentEty (Convex vecs _), colFil :: CollisionFilter, _ :: Not Position)   -> do
            hasPos <- exists parentEty (Proxy :: Proxy Position)
            when hasPos $ 
                get parentEty >>= \(Position pos) -> renderCollision colFil pos vecs
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
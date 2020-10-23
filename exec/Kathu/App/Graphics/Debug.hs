module Kathu.App.Graphics.Debug (renderDebug) where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   hiding (($=))
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (void, when)
import qualified Data.Map                        as Map
import qualified Data.Text                       as T
import qualified Data.Vector.Storable            as SVec
import           SDL                             (($=))
import qualified SDL
import           Verda.Graphics.Color
import           Verda.Graphics.Fonts            (renderText)
import           Verda.Graphics.SpriteManager    (currentPalette)
import           Verda.Util.Containers           (fromJustElseError)
import           Verda.Util.Types                (unID)

import           Kathu.App.System                (SystemT')
import           Kathu.App.Tools.ToolMode        (isNoTool)
import           Kathu.Entity.Physics.CollisionGroup (collisionFilterDebugColor)
import           Kathu.Graphics.Camera
import           Kathu.Language
import           Kathu.World.WorldSpace

-- | A circular shape composed of many vertices; when collisions are drawn, the radius and origin of this can be shifted to match each collision
circleVertices :: SVec.Vector (V2 Double)
circleVertices = SVec.fromList [V2 (x i) (y i) | i <- [0..numPoints]]
    where numPoints = 16 :: Int
          x i = cos $ 2 * pi * fromIntegral i / fromIntegral numPoints
          y i = sin $ 2 * pi * fromIntegral i / fromIntegral numPoints

renderDebug :: SDL.Renderer -> (V2 Double -> V2 Double) -> SystemT' IO ()
renderDebug renderer logicToRenderPos = do
    -- in tool modes collision might be rapidly changing or out-of-date; don't bother to draw
    shouldDrawCol <- isNoTool <$> get global
    when (shouldDrawCol || not shouldDrawCol) $ do
        let drawPoints colFil points = do
                SDL.rendererDrawColor renderer $= unColor (collisionFilterDebugColor colFil)
                -- we render in groups of 4 pixels to have thicker lines
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 0 0)) points
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 0 1)) points
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 1 0)) points
                SDL.drawLines renderer $ SVec.map (SDL.P . (+ V2 1 1)) points
            renderCollision _ _ [] _                = pure ()
            renderCollision colFil pos vecs@(v:_) 0 =
                let convPos vpoint = logicToRenderPos (pos + vpoint)
                    points = SVec.fromList (fmap floor . convPos <$> snoc vecs v)
                 in drawPoints colFil points
            renderCollision colFil pos (origin:_) radius =
                let adjVert = fmap floor . logicToRenderPos . (+pos) . (+origin) . (* V2 radius radius)
                    points  = SVec.map adjVert circleVertices
                 in drawPoints colFil points

        cmapM_ $ \(Shape _ (Convex vecs radius), colFil :: CollisionFilter, Position pos) ->
            renderCollision colFil pos vecs radius
        -- attached boxes don't have a position, as they exist on a separate entity with just a shape
        cmapM_ $ \(Shape parentEty (Convex vecs radius), colFil :: CollisionFilter, _ :: Not Position)   -> do
            hasPos <- exists parentEty (Proxy :: Proxy Position)
            when hasPos $ 
                get parentEty >>= \(Position pos) -> renderCollision colFil pos vecs radius
    renderDebugText renderer

renderDebugText :: SDL.Renderer -> SystemT' IO ()
renderDebugText renderer = do
    world :: WorldSpace <- get global
    manager <- get global
    (V2 camX camY, cZoom) <- cfold (\_ (Position pos, Camera z) -> (pos, z)) (V2 0 0, 1)

    let displayText = T.concat
            [ "palette: ",  T.pack . show . currentPalette $ manager
            , " zoom: ",    T.pack . show $ 1 / cZoom
            , " | world: ", world^.worldID.to unID
            , " x: ",       T.pack . show $ camX
            , " y: ",       T.pack . show $ camY
            ]

    lang      <- get global
    fontCache <- get global
    let smallFont = fromJustElseError "Loaded language is missing \"small\" font definition" . Map.lookup "small" . langFontIDs $ lang
    void $ renderText renderer fontCache smallFont white (V2 0 0) displayText
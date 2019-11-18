{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.App.Graphics.Debug (renderDebug) where

import           Apecs
import           Apecs.Physics
import           Control.Lens             hiding (Identity)
import           Control.Monad            (void, when)
import           Data.Foldable            (foldl')
import qualified Data.Text                as T
import qualified SDL
import qualified SDL.Font                 as SDLF

import           Kathu.App.Data.Library   (font)
import           Kathu.App.Graphics.Image (ImageID)
import           Kathu.App.System         (SystemT')
import           Kathu.Entity.Components  (Identity(..))
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Color
import           Kathu.World.WorldSpace
import           Kathu.Util.Flow          (mapPair)
import           Kathu.Util.Types         (unID)

primaryCollisionBoxColor :: Color
primaryCollisionBoxColor = mkColor 250 50 90 225

attachedCollisionBoxColor :: Color
attachedCollisionBoxColor = mkColor 100 214 110 225

worldCollisionBoxColor :: Color
worldCollisionBoxColor = mkColor 45 170 225 225

renderDebug :: SDL.Surface -> (V2 Double -> V2 Double -> V2 Double) -> SystemT' IO ()
renderDebug screen logicToRenderPos = do
    -- no good way to draw lines when using Surface rendering, so we just draw a box that encapsulates the whole shape
    let boundsOf    = foldl' (\(V2 mnX mnY, V2 mxX mxY) (V2 x y) -> (V2 (min x mnX) (min y mnY), V2 (max x mxX) (max y mxY))) (V2 (1/0) (1/0), V2 (-(1/0)) (-(1/0)))
        -- if the dimensions are zero, we expand them so the box is actually visible
        unZero a | a == 0 = 1 | otherwise = a
        renderBoxes boxColor pos vecs = 
            (flip $ SDL.surfaceFillRect screen) (unColor boxColor)
            . Just . (\(v1, v2) -> SDL.Rectangle (SDL.P v1) (unZero <$> (v2 - v1))) -- transforms two coords into rectangle with origin and dims
            . mapPair (fmap floor . logicToRenderPos pos)
            . boundsOf $ vecs
    -- draws collision boxes
    -- collision placed in world doesn't make use of Identity
    cmapM_ $ \(Position pos, Shape _ (Convex vecs _), _ :: Not Identity) -> renderBoxes worldCollisionBoxColor pos vecs
    -- attached boxes don't have a position, as they exist on a separate entity with just a shape
    cmapM_ $ \(Shape parentEty (Convex vecs _), _ :: Not Position)       -> do
        hasPos <- exists parentEty (Proxy :: Proxy Position)
        when hasPos $
            get parentEty >>= (\(Position pos) -> renderBoxes attachedCollisionBoxColor pos vecs)
    cmapM_ $ \(Position pos, Shape _ (Convex vecs _), _ :: Identity)     -> renderBoxes primaryCollisionBoxColor pos vecs

    renderDebugText screen

renderDebugText :: SDL.Surface -> SystemT' IO ()
renderDebugText screen = do
    world :: WorldSpace ImageID <- get global
    (V2 camX camY) <- cfold (\_ (Position pos, Camera _) -> pos) (V2 0 0)

    let displayText = T.pack . concat $ ["world: ", show (unID . worldID $ world), " x: ", show camX, " y: ", show camY]

    library <- get global
    textSurface       <- SDLF.solid (library^.font) (unColor white) displayText

    void $ SDL.surfaceBlit textSurface Nothing screen (Just . SDL.P $ V2 0 0)
    
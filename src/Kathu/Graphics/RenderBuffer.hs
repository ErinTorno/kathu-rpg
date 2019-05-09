module Kathu.Graphics.RenderBuffer (RenderBuffer, mkRenderBuffer, bufferGrowIncr, sortRenderBuffer) where

import Data.Vector.Algorithms.Intro (sortByBounds)
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVec
import Kathu.Graphics.Drawable (RenderSprite)
import Linear.V3 (V3(..))

-- this stores each sprite to draw and its properties, which we will sort before drawing for z depth
type RenderBuffer = IOVector (V3 Float, Vector RenderSprite)

mkRenderBuffer :: IO RenderBuffer
mkRenderBuffer = MVec.new baseSize
    where baseSize = 1024

bufferGrowIncr :: Int
bufferGrowIncr = 16

sortRenderBuffer :: Int -> Int -> RenderBuffer -> IO ()
sortRenderBuffer min max buf = sortByBounds compRender buf min max

compRender :: (V3 Float, Vector RenderSprite) -> (V3 Float, Vector RenderSprite) -> Ordering
compRender ((V3 _ _ za), _) ((V3 _ _ zb), _) = za `compare` zb
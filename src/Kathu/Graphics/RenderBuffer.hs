module Kathu.Graphics.RenderBuffer (RenderBuffer, mkRenderBuffer, bufferGrowIncr, sortRenderBuffer) where

import Data.Vector.Algorithms.Intro (sortByBounds)
import qualified Data.Vector.Mutable as MVec
import Kathu.Entity.Components (Render)
import Linear.V3 (V3(..))

-- this stores each sprite to draw and its properties, which we will sort before drawing for z depth
type RenderBuffer = MVec.IOVector (V3 Float, Render)

mkRenderBuffer :: IO RenderBuffer
mkRenderBuffer = MVec.new baseSize
    where baseSize = 1024

bufferGrowIncr :: Int
bufferGrowIncr = 16

sortRenderBuffer :: Int -> Int -> RenderBuffer -> IO ()
sortRenderBuffer min max buf = sortByBounds compRender buf min max

compRender :: (V3 Float, Render) -> (V3 Float, Render) -> Ordering
compRender ((V3 _ _ za), _) ((V3 _ _ zb), _) = za `compare` zb
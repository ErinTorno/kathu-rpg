module Kathu.App.Graphics.RenderBuffer
    ( RenderBuffer
    , mkRenderBuffer
    , bufferGrowIncr
    , sortRenderBuffer
    ) where

import           Data.Vector.Algorithms.Intro (sortByBounds)
import           Data.Vector                  (Vector)
import           Data.Vector.Mutable          (IOVector)
import qualified Data.Vector.Mutable          as MVec
import           Linear.V2                    (V2(..))

import           Kathu.App.Graphics.Image     (ImageID)
import           Kathu.Graphics.Drawable      (RenderSprite)

-- this stores each sprite to draw and its properties, which we will sort before drawing for z depth
-- this can be converted to an unboxed MVector if I can find a clean way to drop the RenderSprite
type RenderBuffer = IOVector (V2 Double, Vector (RenderSprite ImageID))

mkRenderBuffer :: IO RenderBuffer
mkRenderBuffer = MVec.new baseSize
    where baseSize = 1024

bufferGrowIncr :: Int
bufferGrowIncr = 32

sortRenderBuffer :: Int -> Int -> RenderBuffer -> IO ()
sortRenderBuffer mn mx buf = sortByBounds compareRender buf mn mx

compareRender :: (V2 Double, Vector (RenderSprite g)) -> (V2 Double, Vector (RenderSprite g)) -> Ordering
compareRender (V2 _ ya, _) (V2 _ yb, _) = ya `compare` yb
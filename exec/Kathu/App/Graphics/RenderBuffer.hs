module Kathu.App.Graphics.RenderBuffer where

import           Control.Monad                (when)
import           Data.IORef
import           Data.Vector.Algorithms.Intro (sortByBounds)
import           Data.Vector.Mutable          (IOVector)
import qualified Data.Vector.Mutable          as MVec
import           Linear.V2                    (V2(..))

import           Kathu.App.Graphics.Image     (ImageID)
import           Kathu.Graphics.Drawable      (RenderSprite, spriteLayer)

type RenderBufferElement = (V2 Double, RenderSprite ImageID)

-- | This stores each sprite to draw and its properties, which will be sorted before drawing for y depth
type RenderBuffer = IORef (IOVector RenderBufferElement)

mkRenderBuffer :: IO RenderBuffer
mkRenderBuffer = newIORef =<< MVec.new baseSize
    where baseSize = 2048

bufferGrowIncr :: Int
bufferGrowIncr = 32

sortRenderBuffer :: Int -> Int -> RenderBuffer -> IO ()
sortRenderBuffer mn mx ref = do
    buf <- readIORef ref
    sortByBounds compareRender buf mn mx

readFromBuffer :: Int -> RenderBuffer -> IO RenderBufferElement
readFromBuffer idx ref = do
    buf <- readIORef ref
    MVec.unsafeRead buf idx

writeToBuffer :: Int -> RenderBufferElement -> RenderBuffer -> IO ()
writeToBuffer idx newElem ref = do
    buf <- readIORef ref
    let bufLen = MVec.length buf
    when (idx >= bufLen) $
        growRenderBuffer (max bufferGrowIncr (idx - bufLen + 1)) ref
    buf' <- readIORef ref
    MVec.unsafeWrite buf' idx newElem

growRenderBuffer :: Int -> RenderBuffer -> IO ()
growRenderBuffer incr ref = do
    buf  <- readIORef ref
    buf' <- MVec.unsafeGrow buf incr
    writeIORef ref buf'

compareRender :: RenderBufferElement -> RenderBufferElement -> Ordering
compareRender a b = pos a `compare` pos b
    where -- when object is on a different layer, we sort it as if it had a much higher position
          pos (V2 _ !y, sprite) = y + 1000 * spriteLayer sprite
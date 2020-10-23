module Verda.Graphics.SpriteBuffer where

import           Control.Monad                (when)
import           Data.IORef
import           Data.Vector.Algorithms.Intro (sortByBounds)
import           Data.Vector.Mutable          (IOVector)
import qualified Data.Vector.Mutable          as MVec
import           Linear.V2                    (V2(..))

import           Verda.Graphics.Color
import           Verda.Graphics.Sprites

data SpriteBufferElement = SpriteBufferElement
    { sbePosition :: !(V2 Double)
    , sbeColor    :: !Color
    , sbeSprite   :: !Sprite
    }

sbeCompare :: SpriteBufferElement -> SpriteBufferElement -> Ordering
sbeCompare a b = pos a `compare` pos b
    where -- when object is on a different layer, we sort it as if it had a much higher position
          pos (SpriteBufferElement (V2 _ !y) _ sprite) = y + 1000 * spriteLayer sprite

-- | This stores each sprite to draw and its properties, which will be sorted before drawing for y depth
type SpriteBuffer = IORef (IOVector SpriteBufferElement)

mkSpriteBuffer :: IO SpriteBuffer
mkSpriteBuffer = newIORef =<< MVec.new baseSize
    where baseSize = 2048

sbeBufferGrowIncr :: Int
sbeBufferGrowIncr = 64

sortSpriteBuffer :: SpriteBuffer -> Int -> Int -> IO ()
sortSpriteBuffer !ref !mn !mx = do
    buf <- readIORef ref
    sortByBounds sbeCompare buf mn mx

sbeRead :: SpriteBuffer -> Int -> IO SpriteBufferElement
sbeRead !ref !idx = do
    buf <- readIORef ref
    MVec.unsafeRead buf idx

sbeWrite :: SpriteBuffer -> Int -> SpriteBufferElement -> IO ()
sbeWrite !ref !idx !newElem = do
    buf <- readIORef ref
    let bufLen = MVec.length buf
    when (idx >= bufLen) $
        growSpriteBuffer ref (max sbeBufferGrowIncr (idx - bufLen + 1))
    buf' <- readIORef ref
    MVec.unsafeWrite buf' idx newElem

growSpriteBuffer :: SpriteBuffer -> Int -> IO ()
growSpriteBuffer !ref !incr = do
    buf  <- readIORef ref
    buf' <- MVec.unsafeGrow buf incr
    writeIORef ref buf'
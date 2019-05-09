module Kathu.Util.MultiDimVector where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec

type Vector2D a = Vector (Vector a)

type Vector3D a = Vector (Vector2D a)

type MVector2D s a = MVector s (MVector s a)

type MVector3D s a = MVector s (MVector2D s a)

type IOVector2D a = MVector2D RealWorld a

type IOVector3D a = MVector3D RealWorld a

-- Construction

new2D :: PrimMonad m => Int -> Int -> m (MVector2D (PrimState m) a)
new2D x y = MVec.new y >>= MVec.replicate x

new2DWith :: PrimMonad m => Int -> Int -> a -> m (MVector2D (PrimState m) a)
new2DWith x y ele = MVec.replicate y ele >>= MVec.replicate x

new3D :: PrimMonad m => Int -> Int -> Int -> m (MVector3D (PrimState m) a)
new3D x y z = MVec.new z >>= MVec.replicate y >>= MVec.replicate x

new3DWith :: PrimMonad m => Int -> Int -> Int -> a -> m (MVector3D (PrimState m) a)
new3DWith x y z ele = MVec.replicate z ele >>= MVec.replicate y >>= MVec.replicate x

fromList2D :: [[a]] -> Vector2D a
fromList2D = Vec.fromList . fmap Vec.fromList

fromList3D :: [[[a]]] -> Vector3D a
fromList3D = Vec.fromList . fmap Vec.fromList . fmap (fmap Vec.fromList)

-- Reading and Writing

read2D :: Int -> Int -> Vector2D a -> a
read2D x y v = (v Vec.! x) Vec.! y

mRead2D :: PrimMonad m => Int -> Int -> MVector2D (PrimState m) a -> m a
mRead2D x y v = MVec.read v x >>= (flip MVec.read) y

write2D :: Int -> Int -> a -> Vector2D a -> Vector2D a
write2D x y ele v = v Vec.// [(x, inner)]
    where inner = (v Vec.! x) Vec.// [(y, ele)]

mWrite2D :: PrimMonad m => Int -> Int -> a -> MVector2D (PrimState m) a -> m ()
mWrite2D x y ele v = MVec.read v x >>= \v' -> MVec.write v' y ele

read3D :: Int -> Int -> Int -> Vector3D a -> a
read3D x y z v = ((v Vec.! x) Vec.! y) Vec.! z

mRead3D :: PrimMonad m => Int -> Int -> Int -> MVector3D (PrimState m) a -> m a
mRead3D x y z v = MVec.read v x >>= (flip MVec.read) y >>= (flip MVec.read) z

write3D :: Int -> Int -> Int -> a -> Vector3D a -> Vector3D a
write3D x y z ele v = v Vec.// [(x, (v Vec.! x) Vec.// [(y, inner)])]
    where inner  = ((v Vec.! x) Vec.! y) Vec.// [(z, ele)]

mWrite3D :: PrimMonad m => Int -> Int -> Int -> a -> MVector3D (PrimState m) a -> m ()
mWrite3D x y z ele v = MVec.read v x >>= (flip MVec.read) y >>= \v' -> MVec.write v' z ele

-- Mapping

miFoldl3D :: PrimMonad m => (b -> Int -> Int -> Int -> a -> m b) -> b -> MVector3D (PrimState m) a -> m b
miFoldl3D f acc v = do
    mX <- pure $ MVec.length v
    mY <- MVec.length <$> (MVec.read v 0)
    mZ <- MVec.length <$> (MVec.read v 0 >>= (flip MVec.read) 0)
    let go x y z b | z == mZ = pure b
                   | y == mY = go 0 0 (z + 1) b
                   | x == mX = go 0 (y + 1) z b
                   | otherwise = mRead3D x y z v >>= f b x y z >>= go (x + 1) y z
    go 0 0 0 acc
module Kathu.Physics.Shapes where

data AABB a = AABB {minX :: a, minY :: a, minZ :: a, maxX :: a, maxY :: a, maxZ :: a} deriving (Show, Eq)

data Sphere a = Sphere {originX :: a, originY :: a, originZ :: a, radius :: a} deriving (Show, Eq)

isAABBIntersect :: Num a => AABB a -> AABB a -> Bool
isAABBIntersect (AABB lx ly lz hx hy hz) (AABB lx' ly' lz' hx' hy' hz') =
    (lx <= hx' && hx >= lx') && (ly <= hy' && hy >= ly') && (lz <= hz' && hz >= lz')
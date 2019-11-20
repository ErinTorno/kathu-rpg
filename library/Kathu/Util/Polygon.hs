{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module Kathu.Util.Polygon
    ( polygonsFromBinaryGrid
    , triangulate
    ) where

import qualified Algorithms.Geometry.PolygonTriangulation.Triangulate as PolyTri

import           Control.Lens
import qualified Data.CircularSeq                      as CSeq
import           Data.Ext
import qualified Data.Foldable                         as F
import           Data.Geometry.PlanarSubdivision.Basic hiding (edges)
import           Data.Geometry.Polygon
import           Data.Geometry.Point
import qualified Data.Geometry.Vector.VectorFamily     as VF
import           Data.List                             (unfoldr)
import           Data.Maybe
import           Data.Vector.Unboxed                   (Vector)
import qualified Data.Vector.Unboxed                   as UVec
import           Linear.V2                             (V2(..))


data PX = PX

data PathDirection = North | East | South | West

triangulate :: (Fractional a, Ord a) => [V2 a] -> [[V2 a]]
triangulate poly = (toV2<$>) <$> points
    where toPoint (V2 x y)              = ext $ point2 x y
          toV2 (Point (VF.Vector2 x y)) = V2 x y
          subdiv    = PolyTri.triangulate (Identity PX) . fromPoints . fmap toPoint $ poly
          facePolys = mapMaybe (^?_2.core._Left) . F.toList . rawFacePolygons $ subdiv
          points    = (\(SimplePolygon cseq) -> F.toList . CSeq.asSeq . fmap _core $ cseq) <$> facePolys

-- Warning: not able to handle enclosed spaces; will need to add in future (flood-fill to determine?)
-- | Takes a vector of Bools, with a width and height to interpret it as, and generates a series of polygons that wraps all groups of Trues
polygonsFromBinaryGrid :: Vector Bool -> Int -> Int -> [[V2 Int]]
polygonsFromBinaryGrid grid !w !h = unfoldr go . sizeCheck $ grid
    where sizeCheck v = if UVec.length v /= w * h then (error . concat) ["polygonsFromBinaryGrid was given ", show w, "x", show h, " as dims, but the Vector had ", show (UVec.length v), " elements in it"] else v
          go v = ((\(x, y) -> let (clearAcc, verts) = edges v x y [] [] East x y in (verts, updateV v clearAcc)) . indToXY) <$> (findNext v 0)
          updateV v clearAcc = UVec.update v (UVec.fromList ((,False) <$> clearAcc))
          xyToInd x y = x + w * y
          indToXY i   = let (y, x) = i `quotRem` w in (x, y)
          check v x y | x < 0 || y < 0 || x >= w || y >= h = False | otherwise = v UVec.! (xyToInd x y)
          findNext v i | i >= w * h = Nothing
                       | v UVec.! i = Just i
                       | otherwise  = findNext v (i + 1)
          -- for each we check point in middle of four-tile region; x-1, y is bottom left, x-1, y-1 is top left, etc.
          -- cacc: clear-accumulator; once we're done, we mark all indices in here as non-solid and try again
          edges :: Vector Bool -> Int -> Int -> [Int] -> [V2 Int] -> PathDirection -> Int -> Int -> ([Int], [V2 Int])
          edges v !sx !sy cacc acc !dir !x !y
              -- first: add then move right; we start at top left-most point, so we will never try to go left or up from this point
              | null acc           = edges v sx sy (xyToInd x y : cacc) (V2 x y : acc) East (x+1) y
              | sx == x && sy == y = (cacc, acc) -- reached starting x and y, so we made a closed path
              | x >= w || y >= h   = moveNext $ edges v sx sy cacc -- don't track in clearAcc, since this point is outside of the grid
              | otherwise          = moveNext $ edges v sx sy (xyToInd x y : cacc)
                  where moveNext f = case dir of
                            -- All implicty assume a certain corner is solid, as the direction couldn't have been reached with it being so on a previous iteration
                            -- We then need to check its diagonal first to see if we need to change directions, as otherwise the path could clip across solid diagonals
                            North -> if | check v (x-1) (y-1) -> f (V2 x y : acc) West (x-1) y
                                        | check v x (y-1)     -> f acc North x (y-1) -- same dir, so no add point to path
                                        | otherwise           -> f (V2 x y : acc) East (x+1) y
                            East  -> if | check v x (y-1)     -> f (V2 x y : acc) North x (y-1)
                                        | check v x y         -> f acc East (x+1) y
                                        | otherwise           -> f (V2 x y : acc) South x (y+1)
                            South -> if | check v x y         -> f (V2 x y : acc) East (x+1) y
                                        | check v (x-1) y     -> f acc South x (y+1)
                                        | otherwise           -> f (V2 x y : acc) West (x-1) y
                            West  -> if | check v (x-1) y     -> f (V2 x y : acc) South x (y+1)
                                        | check v (x-1) (y-1) -> f acc West (x-1) y
                                        | otherwise           -> f (V2 x y : acc) North x (y-1)
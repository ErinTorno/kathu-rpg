module Kathu.Util.Polygon
    ( Polygon(..)
    , mapPolyList
    , mapPolyVertices
    , isPolygonInside
    , convexAndConcaveFromBinaryGrid
    , polygonsFromBinaryGrid
    , triangulate
    ) where

import qualified Algorithms.Geometry.PolygonTriangulation.Triangulate as PolyTri

import qualified Data.Bifunctor                        as Bi
import           Control.Lens
import qualified Data.CircularSeq                      as CSeq
import           Data.Ext
import qualified Data.Foldable                         as F
import           Data.Geometry.PlanarSubdivision.Basic hiding (edges)
import qualified Data.Geometry.Polygon                 as Poly
import           Data.Geometry.Point
import qualified Data.Geometry.Vector.VectorFamily     as VF
import           Data.Maybe
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Vector                           (Vector)
import qualified Data.Vector                           as Vec
import qualified Data.Vector.Unboxed                   as UVec
import           Linear.V2                             (V2(..))


-- Fairly long, complicated, unsafe, and awful code
-- This is mostly used to generate monotone polygon collision boxes for the tiles in a world
-- Most of the function in this make a lot of assumptions that can fail if the logic of the other functions is changed (such as clockwise vs counterclockwise, non-empty lists, etc.)

data Polygon a = Polygon {polyBorder :: ![V2 a], polyHoles :: ![[V2 a]]} deriving (Show, Eq)

mapPolyList :: ([V2 a] -> [V2 b]) -> Polygon a -> Polygon b
mapPolyList f (Polygon outer inner) = Polygon (f outer) (f <$> inner)

mapPolyVertices :: (V2 a -> V2 b) -> Polygon a -> Polygon b
mapPolyVertices f (Polygon outer inner) = Polygon (f <$> outer) (fmap f <$> inner)

instance Functor Polygon where
    fmap f (Polygon outer inner) = Polygon (fmap f <$> outer) (fmap (fmap f) <$> inner)

--------------
-- Internal --
--------------

data PX = PX

data PathDirection = North | East | South | West

data VertexPath = ExteriorPath [V2 Int] | InteriorPath [V2 Int] deriving Show

pathVertices :: VertexPath -> [V2 Int]
pathVertices (InteriorPath v) = v
pathVertices (ExteriorPath v) = v

isExterior :: VertexPath -> Bool
isExterior (ExteriorPath _) = True
isExterior _                = False

-- | A segment that with an initial and final position along a single axis, and the entire segments position on the orthogonal axis
data Segment = Segment {segOrthogonal :: Int, segStart :: Int, segEnd :: Int} deriving (Eq, Show)

-- | A collection of axis-aligned segments, both horizontal and vertical
data AASegments = AASegments {hSegments :: [Segment], vSegments :: [Segment]} deriving (Eq, Show)

toAASegments :: [V2 Int] -> AASegments
toAASegments []     = AASegments [] []
toAASegments (p:ps) = uncurry AASegments $ go p ps
    where -- We want all segment ends to be >= the start for easier comparison
          mkIncrSeg !o !a !b = if a < b then Segment o a b else Segment o b a
          go _ [] = ([], [])
          go (V2 x y) (next@(V2 x' y'):vs)
              | x == x'   = Bi.first  (mkIncrSeg x y y' :) $ go next vs
              | y == y'   = Bi.second (mkIncrSeg y x x' :) $ go next vs
              | otherwise = error "Attempted to create Lines from non-axis-aligned vertices"

isAAInside :: AASegments -> AASegments -> Bool
isAAInside (AASegments innerH innerV) (AASegments outerH outerV) = checkAll innerH outerV && checkAll innerV outerH
    where -- even-odd rule for determining inside
          -- for each segment, we check for intersections from rays in both directions
          checkAll (seg:segs) outer = check secondCheck $ interCount True seg outer
              where secondCheck     = check (checkAll segs outer) $ interCount False seg outer
          checkAll [] _             = False
          check f !iCount
              | iCount == 0         = f
              | iCount `mod` 2 /= 0 = True  -- we intersected an odd amount of segments with this ray, so definitely inside this shape 
              | otherwise           = False -- if we intersect an even amount, then we can't inside this shape
          -- extends a ray out and determines if that ray would intersect the orthogonal segment
          rayCrosses isPosDir (Segment segPos segI _) (Segment orthoPos orthoI orthoF) = segPos >= orthoI && segPos <= orthoF && corSide
              where corSide = if isPosDir then segI < orthoPos else segI > orthoPos
          -- counts the number of orthogonal segments a cast ray would intersect with
          interCount :: Bool -> Segment -> [Segment] -> Int
          interCount !isPosDir seg = F.foldl' appendIf 0
              where appendIf !count ortho = count + (if rayCrosses isPosDir seg ortho then 1 else 0)

-- | Transforms a list of VertexPaths into a list of Nodes containing detailed information about each path, organized into a tree based on what paths contain other paths
mkPolygons :: Vector VertexPath -> Vector (Polygon Int)
mkPolygons paths
    | Vec.null paths = Vec.empty
    | otherwise      = Vec.unfoldr checkNext (0, interiorsInIdxs, Set.empty)
    where checkNext (idx, curInteriors, done)
              | canEnd                = Nothing
              | Vec.null curInteriors = Just (Polygon extPath [], (nextIdx, curInteriors, Set.insert idx done))
              | Vec.null directIn     = checkNext (nextIdx, notDirect, done) -- nothing we can take yet, so we immediately continue on
              | otherwise             = Just (Polygon extPath directInCleaned, (nextIdx, notDirect, Set.insert idx done))
              where canEnd                = Vec.null curInteriors && Set.size done == exteriorCount
                    nextIdx               = (idx + 1) `mod` exteriorCount
                    extPath               = exteriorsV Vec.! idx
                    -- the Set doesn't do anything after it becomes empty, so we drop it
                    directInCleaned       = Vec.toList . Vec.map fst $ directIn
                    -- for this idx, we remove it from all containing sets; sets that are now empty must be a direct interior to the given exterior
                    -- ones that still have more containing are then not direct, and so we can continue to check later
                    (directIn, notDirect) = Vec.partition (\(_,  idxs) -> Set.null idxs)
                                          . Vec.map (\(p, idxs) -> (p, Set.delete idx idxs))
                                          $ curInteriors

          (exteriors, interiors) = Vec.partition isExterior paths
          exteriorsV             = pathVertices <$> exteriors
          interiorsV             = pathVertices <$> interiors
          exteriorsAA            = toAASegments . pathVertices <$> exteriors
          exteriorCount          = Vec.length exteriorsV

          interiorsInIdxs :: Vector ([V2 Int], Set Int)
          interiorsInIdxs = Vec.map (\p -> (p, isInsideAll . toAASegments $ p)) interiorsV
              where isInsideAll inAA = Vec.ifoldl' (\acc i extAA -> if inAA `isAAInside` extAA then Set.insert i acc else acc) Set.empty exteriorsAA

--------------
-- Exported --
--------------

-- Mostly here for tests around the private isAAInside
isPolygonInside :: [V2 Int] -> [V2 Int] -> Bool
isPolygonInside inner outer = isAAInside (toAASegments inner) (toAASegments outer)

triangulate :: (Fractional a, Ord a) => Polygon a -> [[V2 a]]
triangulate (Polygon outer inner)
    | null inner = fmap toV2 <$> (points . simplePoly $ outer)
    | otherwise  = filterHoles (fmap toV2 <$> points (Poly.MultiPolygon (CSeq.fromList . map toPoint $ outer) (simplePoly <$> inner)))
    where toPoint (V2 x y)              = ext $ Point2 x y
          toV2 (Point (VF.Vector2 x y)) = V2 x y

          -- sometimes (always?) triangulate includes all of the holes in the new triangulation too, so we have to remove
          filterHoles = let holeSet = Set.fromList (Set.fromList <$> inner) in filter ((`Set.notMember`holeSet) . Set.fromList) 

          simplePoly = Poly.fromPoints . fmap toPoint

          facePolys  :: (Ord a, Fractional a) => Poly.Polygon t p a -> [Poly.Polygon 'Poly.Simple p a]
          facePolys   = mapMaybe (^?_2.core._Left) . F.toList . rawFacePolygons . PolyTri.triangulate (Identity PX)
          points     :: (Ord a, Fractional a) => Poly.Polygon t p a -> [[Point 2 a]]
          points poly = (\(Poly.SimplePolygon cseq) -> F.toList . CSeq.asSeq . fmap _core $ cseq) <$> facePolys poly

convexAndConcaveFromBinaryGrid :: UVec.Vector Bool -> Int -> Int -> (Vector (Polygon Int), Vector (Polygon Int))
convexAndConcaveFromBinaryGrid grid w = Vec.partition check . polygonsFromBinaryGrid grid w
    where check (Polygon outer inner) = null inner && length outer <= 4
-- due to how our tracing works, we know that all 4-point polygons are rectangles, and any 5-point or above must be concave in some way

-- | Takes a vector of Bools, with a width and height to interpret it as, and generates a series of polygons that wraps all groups of Trues
polygonsFromBinaryGrid :: UVec.Vector Bool -> Int -> Int -> Vector (Polygon Int)
polygonsFromBinaryGrid v !w !h
    | UVec.length v /= w * h = error errMsg
    | otherwise              = mkPolygons $ Vec.unfoldr go Set.empty
    where -- from a given list of already read points, we generate a new shape if possible
          go readPoints = mkEdges readPoints <$> findNext readPoints 0
          mkEdges rp (isInverse, V2 x y) = (verts, Set.union rp clearAcc)
              where (clearAcc, verts) = edges isInverse x y Set.empty [] East x y

          errMsg = concat ["polygonsFromBinaryGrid was given ", show w, "x", show h, " as dims, but the Vector had ", show $ UVec.length v, " elements in it"]

          xyToInd !x !y = x + w * y
          indToXY !i    = let (y, x) = i `quotRem` w in V2 x y

          check !x !y | x < 0 || y < 0 || x >= w || y >= h = False
                      | otherwise                          = v UVec.! xyToInd x y
          findNext :: Set (V2 Int) -> Int -> Maybe (Bool, V2 Int)
          findNext rp i | i >= w * h             = Nothing
                        | canStart && notAlready = Just (isInverse, indToXY i)
                        | otherwise              = findNext rp (i + 1)
                        where canStart   = v UVec.! i && (i `mod` w == 0 || not (v UVec.! (i - 1))) -- tile to its left must be empty, and at right must be present
                              notAlready = Set.notMember (indToXY i) rp
                              isInverse  = (i >= w) && (v UVec.! (i - w)) -- if above is solid and this hasn't been marked, it is a hole in another shape

          -- for each we check point in middle of four-tile region; x-1, y is bottom left, x-1, y-1 is top left, etc.
          -- cacc: clear-accumulator; once we're done, we mark all indices in here as non-solid and try again
          edges :: Bool -> Int -> Int -> Set (V2 Int) -> [V2 Int] -> PathDirection -> Int -> Int -> (Set (V2 Int), VertexPath)
          edges !isInverse !sx !sy cacc acc dir !x !y
              -- first: add then move right; we start at top left-most point, so we will never try to go left or up from this point
              -- if isInverse we start moving south instead since this is a "hole", and can't move East due to having a solid space above it
              | null acc           = let f = edges isInverse sx sy (Set.insert (V2 x y) cacc) (V2 x y : acc)
                                      in if isInverse then f South x (y+1) else f East (x+1) y
              | sx == x && sy == y = (cacc, if isInverse then InteriorPath acc else ExteriorPath acc) -- reached starting x and y, so we made a closed path
              | otherwise          = moveNext $ edges isInverse sx sy (Set.insert (V2 x y) cacc)
                  where moveNext f = if isInverse then moveInverse f else moveNormal f
                        moveNormal f = case dir of
                            -- All implicty assume a certain corner is solid, as the direction couldn't have been reached with it being so on a previous iteration
                            -- We then need to check its diagonal first to see if we need to change directions, as otherwise the path could clip across solid diagonals
                            North -> if | check (x-1) (y-1) -> f (V2 x y : acc) West  (x-1) y
                                        | check x (y-1)     -> f           acc  North  x   (y-1) -- same dir, so no add point to path
                                        | otherwise         -> f (V2 x y : acc) East  (x+1) y
                            East  -> if | check x (y-1)     -> f (V2 x y : acc) North  x   (y-1)
                                        | check x y         -> f           acc  East  (x+1) y
                                        | otherwise         -> f (V2 x y : acc) South  x   (y+1)
                            South -> if | check x y         -> f (V2 x y : acc) East  (x+1) y
                                        | check (x-1) y     -> f           acc  South  x   (y+1)
                                        | otherwise         -> f (V2 x y : acc) West  (x-1) y
                            West  -> if | check (x-1) y     -> f (V2 x y : acc) South  x   (y+1)
                                        | check (x-1) (y-1) -> f           acc  West  (x-1) y
                                        | otherwise         -> f (V2 x y : acc) North  x   (y-1)
                        moveInverse f = case dir of
                            -- same as above, except assumed corner is mirrored
                            North -> if | check x (y-1)     -> f (V2 x y : acc) East  (x+1) y
                                        | check (x-1) (y-1) -> f           acc  North  x   (y-1)
                                        | otherwise         -> f (V2 x y : acc) West  (x-1) y
                            East  -> if | check x y         -> f (V2 x y : acc) South  x   (y+1)
                                        | check x (y-1)     -> f           acc  East  (x+1) y
                                        | otherwise         -> f (V2 x y : acc) North  x   (y-1)
                            South -> if | check (x-1) y     -> f (V2 x y : acc) West  (x-1) y
                                        | check x y         -> f           acc  South  x   (y+1)
                                        | otherwise         -> f (V2 x y : acc) East  (x+1) y
                            West  -> if | check (x-1) (y-1) -> f (V2 x y : acc) North  x   (y-1)
                                        | check (x-1) y     -> f           acc  West  (x-1) y
                                        | otherwise         -> f (V2 x y : acc) South  x   (y+1)

module Kathu.Test.Util (utilTests) where

import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Vector         as Vec
import qualified Data.Vector.Unboxed as UVec
import           Linear.V2           (V2(..))
import           Test.HUnit

import           Kathu.Util.Polygon

utilTests :: Test
utilTests = TestList
    [ TestLabel "triangulate Simple" triangulateSimple
    , TestLabel "triangulate One-Hole" triangulateWithOneHole
    , TestLabel "polygonsFromBinaryGrid Empty" fromGridEmpty
    , TestLabel "polygonsFromBinaryGrid Full" fromGridFull
    , TestLabel "polygonsFromBinaryGrid One Group" fromGridOnePolygonGroup
    , TestLabel "polygonsFromBinaryGrid Two Groups" fromGridTwoPolygonGroup
    , TestLabel "polygonsFromBinaryGrid Snake Shape" fromGridSnakeShape
    , TestLabel "polygonsFromBinaryGrid One-Hole Shape" fromGridHollowPolygonWithOneHole
    , TestLabel "polygonsFromBinaryGrid Two-Hole Horizontal Shape" fromGridHollowPolygonWithTwoHorizontalHoles
    , TestLabel "isInsidePolygon Rectangles True" isInsideRectangles
    , TestLabel "isInsidePolygon Rectangles False by +x" isInsideRectanglesNotByPosX
    , TestLabel "isInsidePolygon Rectangles False by -x" isInsideRectanglesNotByNegX
    , TestLabel "isInsidePolygon Rectangles False by +y" isInsideRectanglesNotByPosY
    , TestLabel "isInsidePolygon Rectangles False by -y" isInsideRectanglesNotByNegY
    , TestLabel "isInsidePolygon U-Shape True top left" isInsideUShapeTopLeft
    , TestLabel "isInsidePolygon U-Shape True top right" isInsideUShapeTopRight
    , TestLabel "isInsidePolygon U-Shape True bottom" isInsideUShapeBottom
    , TestLabel "isInsidePolygon U-Shape False outside by +x" isInsideUShapeOutside
    ]

-------------
-- Polygon --
-------------

mkBoolGrid :: [Int] -> UVec.Vector Bool
mkBoolGrid = UVec.fromList . map (/=0)

triangulateSimple :: Test
triangulateSimple = TestCase $ assertEqual "Simple polygon was triangulated" expected (Set.fromList . triangulate $ polygon)
    where expected :: Set [V2 Double]
          expected = Set.fromList [[V2 1 1, V2 2 0, V2 0 0], [V2 0 2, V2 1 1, V2 0 0], [V2 1 2, V2 1 1, V2 0 2], [V2 2 1, V2 2 0, V2 1 1]]
          polygon  = Polygon [V2 0 0, V2 2 0, V2 2 1, V2 1 1, V2 1 2, V2 0 2] []

triangulateWithOneHole :: Test
triangulateWithOneHole = TestCase $ assertEqual "Multi polygon with one hole was triangulated" expected (Set.fromList . triangulate $ polygon)
    where expected :: Set [V2 Double]
          expected = Set.fromList [ [V2 2 1, V2 3 0, V2 0 0]
                                  , [V2 1 1, V2 2 1, V2 0 0]
                                  , [V2 0 3, V2 1 1, V2 0 0]
                                  , [V2 3 3, V2 1 2, V2 0 3]
                                  , [V2 1 2, V2 1 1, V2 0 3]
                                  , [V2 3 3, V2 2 2, V2 1 2]
                                  , [V2 3 3, V2 3 0, V2 2 1]
                                  , [V2 2 2, V2 3 3, V2 2 1] ]
          polygon = Polygon [V2 0 3, V2 3 3, V2 3 0, V2 0 0] [[V2 1 1, V2 1 2, V2 2 2, V2 2 1]]
          --polygon  = Polygon [V2 0 0, V2 3 0, V2 3 3, V2 0 3] [[V2 1 1, V2 2 1, V2 2 2, V2 1 2]]

fromGridEmpty :: Test
fromGridEmpty = TestCase $ assertEqual "An empty grid has no polygons" Vec.empty (polygonsFromBinaryGrid grid 2 2)
    where grid = mkBoolGrid $ [0, 0, 0, 0]

fromGridFull :: Test
fromGridFull = TestCase $ assertEqual "A full grid has one polygons" expected (polygonsFromBinaryGrid grid 3 3)
    where expected = Vec.singleton (Polygon (reverse [V2 0 0, V2 3 0, V2 3 3, V2 0 3]) [])
          grid     = mkBoolGrid . take 9  $ repeat 1

fromGridOnePolygonGroup :: Test
fromGridOnePolygonGroup = TestCase $ assertEqual "One group of True is converted to a single polygon" expected results
    where results  = mapPolyList reverse <$> polygonsFromBinaryGrid grid 3 3 -- we reverse as it generates counter-clockwise
          expected = Vec.singleton (Polygon [V2 0 0, V2 3 0, V2 3 2, V2 2 2, V2 2 1, V2 0 1] [])
          grid = mkBoolGrid $
                     [ 1, 1, 1
                     , 0, 0, 1
                     , 0, 0, 0
                     ]

fromGridTwoPolygonGroup :: Test
fromGridTwoPolygonGroup = TestCase $ assertEqual "Two groups of True is converted to a two polygons" expected results
    where results  = mapPolyList reverse <$> polygonsFromBinaryGrid grid 3 3 -- we reverse as it generates counter-clockwise
          expected = Vec.fromList [Polygon [V2 0 0, V2 3 0, V2 3 2, V2 2 2, V2 2 1, V2 0 1] [], Polygon [V2 0 2, V2 1 2, V2 1 3, V2 0 3] []]
          grid = mkBoolGrid $
                     [ 1, 1, 1
                     , 0, 0, 1
                     , 1, 0, 0
                     ]

fromGridSnakeShape :: Test
fromGridSnakeShape = TestCase $ assertEqual "One long winding path is converted to a single long shape" expected results
    where results  = mapPolyList reverse <$> polygonsFromBinaryGrid grid 7 5 -- we reverse as it generates counter-clockwise
          expected = Vec.singleton (Polygon [V2 1 0, V2 7 0, V2 7 4, V2 4 4, V2 4 3, V2 1 3, V2 1 4, V2 3 4, V2 3 5, V2 0 5, V2 0 2, V2 5 2, V2 5 3, V2 6 3, V2 6 1, V2 1 1] [])
          grid = mkBoolGrid $
                     [ 0, 1, 1, 1, 1, 1, 1
                     , 0, 0, 0, 0, 0, 0, 1
                     , 1, 1, 1, 1, 1, 0, 1
                     , 1, 0, 0, 0, 1, 1, 1
                     , 1, 1, 1, 0, 0, 0, 0
                     ]

fromGridHollowPolygonWithOneHole :: Test
fromGridHollowPolygonWithOneHole = TestCase $ assertEqual "One hollow grouping is converted to a hollow shape" expected results
    where results  = polygonsFromBinaryGrid grid 3 3 -- no need to reverse as shapes with interiors are generated clockwise
          expected = Vec.singleton (Polygon [V2 0 3, V2 3 3, V2 3 0, V2 0 0] [[V2 1 1, V2 1 2, V2 2 2, V2 2 1]])
          grid = mkBoolGrid $
                     [ 1, 1, 1
                     , 1, 0, 1
                     , 1, 1, 1
                     ]

fromGridHollowPolygonWithTwoHorizontalHoles :: Test
fromGridHollowPolygonWithTwoHorizontalHoles = TestCase $ assertEqual "One hollow grouping with two horizontal holes is converted to a hollow shape" expected results
    where results  = polygonsFromBinaryGrid grid 5 3 -- no need to reverse as shapes with interiors are generated clockwise
          expected = Vec.singleton (Polygon [V2 0 3, V2 5 3, V2 5 0, V2 0 0] [[V2 1 1, V2 1 2, V2 2 2, V2 2 1], [V2 3 1, V2 3 2, V2 4 2, V2 4 1]])
          grid = mkBoolGrid $
                     [ 1, 1, 1, 1, 1
                     , 1, 0, 1, 0, 1
                     , 1, 1, 1, 1, 1
                     ]

---------------------------
-- isInsidePolygon tests --
---------------------------

isInsideRectangles :: Test
isInsideRectangles = TestCase $ assertEqual "Rectangle is inside other one" True (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 0 3]
          inner = [V2 1 1, V2 2 1, V2 2 2, V2 1 2]

isInsideRectanglesNotByPosX :: Test
isInsideRectanglesNotByPosX = TestCase $ assertEqual "Rectangle is not inside other one; shifted + along x" False (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 0 3]
          inner = [V2 4 1, V2 5 1, V2 5 2, V2 4 2]

isInsideRectanglesNotByNegX :: Test
isInsideRectanglesNotByNegX = TestCase $ assertEqual "Rectangle is not inside other one; shifted + along x" False (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 0 3]
          inner = [V2 (-2) 1, V2 (-1) 1, V2 (-1) 2, V2 (-2) 2]

isInsideRectanglesNotByPosY :: Test
isInsideRectanglesNotByPosY = TestCase $ assertEqual "Rectangle is not inside other one; shifted + along x" False (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 0 3]
          inner = [V2 4 1, V2 5 1, V2 5 2, V2 4 2]

isInsideRectanglesNotByNegY :: Test
isInsideRectanglesNotByNegY = TestCase $ assertEqual "Rectangle is not inside other one; shifted + along x" False (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 0 3]
          inner = [V2 (-2) 1, V2 (-1) 1, V2 (-1) 2, V2 (-2) 2]

-- U-Shape

isInsideUShapeTopLeft :: Test
isInsideUShapeTopLeft = TestCase $ assertEqual "Rectangle is inside U shape top left" True (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 4 3, V2 4 0, V2 7 0, V2 7 5, V2 0 5]
          inner = [V2 1 1, V2 2 1, V2 2 2, V2 1 2]

isInsideUShapeTopRight :: Test
isInsideUShapeTopRight = TestCase $ assertEqual "Rectangle is inside U shape top right" True (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 4 3, V2 4 0, V2 7 0, V2 7 5, V2 0 5]
          inner = [V2 5 1, V2 6 1, V2 6 2, V2 5 2]

isInsideUShapeBottom :: Test
isInsideUShapeBottom = TestCase $ assertEqual "Rectangle is inside U shape bottom" True (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 4 3, V2 4 0, V2 7 0, V2 7 6, V2 0 6]
          inner = [V2 1 4, V2 6 4, V2 6 5, V2 1 5]

isInsideUShapeOutside :: Test
isInsideUShapeOutside = TestCase $ assertEqual "Rectangle is not inside U shape by +x shift" False (inner `isPolygonInside` outer)
    where outer = [V2 0 0, V2 3 0, V2 3 3, V2 4 3, V2 4 0, V2 7 0, V2 7 5, V2 0 5]
          inner = [V2 8 1, V2 9 1, V2 9 2, V2 8 2]


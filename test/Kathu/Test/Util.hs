module Kathu.Test.Util (utilTests) where

import qualified Data.Vector.Unboxed as UVec
import           Linear.V2           (V2(..))
import           Test.HUnit

import           Kathu.Util.Polygon

utilTests :: Test
utilTests = TestList
    [ TestLabel "triangulate Simple" triangulateSimple
    , TestLabel "polygonsFromBinaryGrid Empty" fromGridEmpty
    , TestLabel "polygonsFromBinaryGrid One Group" fromGridOnePolygonGroup
    , TestLabel "polygonsFromBinaryGrid Two Groups" fromGridTwoPolygonGroup
    , TestLabel "polygonsFromBinaryGrid Snake Shape" fromGridSnakeShape
    ]

-------------
-- Polygon --
-------------

triangulateSimple :: Test
triangulateSimple = TestCase $ assertEqual "Simple polygon was triangulated" expected (triangulate polygon)
    where expected :: [[V2 Double]]
          expected = [[V2 1 1, V2 2 0, V2 0 0], [V2 0 2, V2 1 1, V2 0 0], [V2 1 2, V2 1 1, V2 0 2], [V2 2 1, V2 2 0, V2 1 1]]
          polygon  = [V2 0 0, V2 2 0, V2 2 1, V2 1 1, V2 1 2, V2 0 2]

fromGridEmpty :: Test
fromGridEmpty = TestCase $ assertEqual "One group of Bools is converted to a single polygon" [] (polygonsFromBinaryGrid grid 2 2)
    where grid = UVec.fromList . fmap (/=0) $ [0, 0, 0, 0]

fromGridOnePolygonGroup :: Test
fromGridOnePolygonGroup = TestCase $ assertEqual "One group of Bools is converted to a single polygon" expected results
    where results  = reverse <$> polygonsFromBinaryGrid grid 3 3 -- we reverse as it generates counter-clockwise
          expected = [[V2 0 0, V2 3 0, V2 3 2, V2 2 2, V2 2 1, V2 0 1]]
          grid = UVec.fromList . fmap (/=0) $
                     [ 1, 1, 1
                     , 0, 0, 1
                     , 0, 0, 0
                     ]

fromGridTwoPolygonGroup :: Test
fromGridTwoPolygonGroup = TestCase $ assertEqual "Two groups of Bools is converted to a two polygons" expected results
    where results  = reverse <$> polygonsFromBinaryGrid grid 3 3 -- we reverse as it generates counter-clockwise
          expected = [[V2 0 0, V2 3 0, V2 3 2, V2 2 2, V2 2 1, V2 0 1], [V2 0 2, V2 1 2, V2 1 3, V2 0 3]]
          grid = UVec.fromList . fmap (/=0) $
                     [ 1, 1, 1
                     , 0, 0, 1
                     , 1, 0, 0
                     ]

fromGridSnakeShape :: Test
fromGridSnakeShape = TestCase $ assertEqual "Two groups of Bools is converted to a two polygons" expected results
    where results  = reverse <$> polygonsFromBinaryGrid grid 7 5 -- we reverse as it generates counter-clockwise
          expected = [[V2 1 0, V2 7 0, V2 7 4, V2 4 4, V2 4 3, V2 1 3, V2 1 4, V2 3 4, V2 3 5, V2 0 5, V2 0 2, V2 5 2, V2 5 3, V2 6 3, V2 6 1, V2 1 1]]
          grid = UVec.fromList . fmap (/=0) $
                     [ 0, 1, 1, 1, 1, 1, 1
                     , 0, 0, 0, 0, 0, 0, 1
                     , 1, 1, 1, 1, 1, 0, 1
                     , 1, 0, 0, 0, 1, 1, 1
                     , 1, 1, 1, 0, 0, 0, 0
                     ]


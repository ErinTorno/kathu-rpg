module EntityTests (entityTests) where

import Kathu.Entity.Action
import Test.HUnit

entityTests :: Test
entityTests = TestList
    [ TestLabel "Direction Conversion North" directionConvN
    ]

------------
-- Action --
------------

directionConvN :: Test
directionConvN = TestCase $ assertEqual "North direction is preserved" (Just North) (indexToDirection (iNorth :: Int))
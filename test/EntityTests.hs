module EntityTests (entityTests) where

import Kathu.Entity.Action
import Linear.V3 (V3(..))
import Test.HUnit

entityTests = TestList
    [ TestLabel "Direction Conversion North" directionConvN
    ]

------------
-- Action --
------------

directionConvN = TestCase $ assertEqual "North direction is preserved" (Just North) (intToDir iNorth)
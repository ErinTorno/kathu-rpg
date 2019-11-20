module Kathu.Test.Entity (entityTests) where

import Test.HUnit

import Kathu.Entity.Action

entityTests :: Test
entityTests = TestList
    [ TestLabel "Direction Conversion North" directionConvN
    ]

------------
-- Action --
------------

directionConvN :: Test
directionConvN = TestCase $ assertEqual "North direction is preserved" (Just North) (indexToDirection (iNorth :: Int))
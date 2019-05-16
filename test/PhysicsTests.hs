module PhysicsTests (physicsTests) where

import Kathu.Physics.Body
import Linear.V3 (V3(..))
import Test.HUnit

physicsTests = TestList
    [ TestLabel "Collision 1" collision1
    , TestLabel "Collision 2" collision2
    , TestLabel "Collision 3" collision3
    , TestLabel "Collision 4" collision4
    ]

---------------
-- Collision --
---------------

collision1 = TestCase (assertEqual msg1 True (isColliding b1 b2) >> assertEqual msg2 True (isColliding b2 b1))
    where b1   = Body (V3 0 0 0) (V3 1 1 1) Static 0
          b2   = Body (V3 0.5 0.5 0.5) (V3 1.5 1.5 1.5) Static 0
          msg1 = "collision for 1x1x1 cubes at (0, 0, 0) and (0.5, 0.5, 0.5)"
          msg2 = "collision with reverse params"
   
collision2 = TestCase (assertEqual msg1 False (isColliding b1 b2) >> assertEqual msg2 False (isColliding b2 b1))
    where b1   = Body (V3 0 0 0) (V3 1 1 1) Static 0
          b2   = Body (V3 1 1 1) (V3 2 2 2) Static 0
          msg1 = "collision for 1x1x1 cubes at (0, 0, 0) and (1, 1, 1)"
          msg2 = "collision with reverse params"

collision3 = TestCase (assertEqual msg1 True (isColliding b1 b2) >> assertEqual msg2 True (isColliding b2 b1))
    where b1   = Body (V3 0 0 0) (V3 3 3 3) Static 0
          b2   = Body (V3 1 1 1) (V3 2 2 2) Static 0
          msg1 = "collision for cube encapsulating another"
          msg2 = "collision with reverse params"

collision4 = TestCase (assertEqual msg1 False (isColliding b1 b2) >> assertEqual msg2 False (isColliding b2 b1))
    where b1   = Body (V3 0 0 0) (V3 1 1 1) Static 0
          b2   = Body (V3 3 3 3) (V3 4 4 4) Static 0
          msg1 = "collision for 1x1x1 cubes at (0, 0, 0) and (3, 3, 3)"
          msg2 = "collision with reverse params"
module Kathu.Physics.PhysSystem where

import Apecs
import Data.Word
import Linear.V3 (V3(..))

data CollideBehavior = Static | Dynamic Float | Bounds deriving (Show, Eq)

data Body w = Body
    { bodyPosition :: V3 Float
    , bodyDimensions :: V3 Float
    , collideBehavior :: CollideBehavior
    , collisionTeams :: Word32
    , parentEty :: Entity
    , onCollide :: Entity -> Entity -> System w ()
    }

nothingOnCollide :: Entity -> Entity -> System w ()
nothingOnCollide _ _ = pure ()


module Kathu.Physics.Body where

import Data.Word
import Linear.V3 (V3(..))

data CollideBehavior = Static | Dynamic Float | Bounds deriving (Show, Eq)

data Body e = Body
    { bodyPosition :: V3 Float
    , bodyDimensions :: V3 Float
    , collideBehavior :: CollideBehavior
    , collisionTeams :: Word32
    , parentEty :: e
    -- ideally we would like the following, but it would have circular results
    -- where we need to know the world to create the type that is used to create the world
    -- , onCollision :: Entity -> Entity -> SystemT w m ()
    }
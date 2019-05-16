{-# LANGUAGE BangPatterns #-}

module Kathu.Physics.Body where

import Data.Word
import Linear.V3 (V3(..))

data CollideBehavior = Static | Dynamic Float | Bounds deriving (Show, Eq)

data Body = Body
    { topLeftPos :: {-# UNPACK #-} !(V3 Float)
    , bottomRightPos :: {-# UNPACK #-} !(V3 Float)
    , collideBehavior :: CollideBehavior
    , collisionTeams :: Word32
    -- ideally we would like the following, but it would have circular results
    -- where we need to know the world to create the type that is used to create the world
    -- , onCollision :: Entity -> Entity -> SystemT w m ()
    }

isColliding :: Body -> Body -> Bool
isColliding ba bb = isInX && isInY && isInZ
    where ((V3 ax ay az), (V3 ax' ay' az')) = (topLeftPos ba, bottomRightPos ba)
          ((V3 bx by bz), (V3 bx' by' bz')) = (topLeftPos bb, bottomRightPos bb)
          isInX = (ax > bx && ax < bx') || (bx > ax && bx < ax')
          isInY = (ay > by && ay < by') || (by > ay && by < ay')
          isInZ = (az > bz && az < bz') || (bz > az && bz < az')
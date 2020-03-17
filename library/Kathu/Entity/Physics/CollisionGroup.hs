{-# LANGUAGE OverloadedStrings #-}

module Kathu.Entity.Physics.CollisionGroup
    ( CollisionGroup(..)
    , mkGroupSensor
    , groupCollisionFilter
    , movementFilter
    , movementSensorFilter
    , floorEffectFilter
    , interactFilter
    , hitboxFilter
    , attackFilter
    ) where

import           Apecs.Physics    (maskList, CollisionFilter(..), Sensor(..))
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Vector      (Vector)
import qualified Data.Vector      as Vec

-- movement is kept separate from hitboxes for all objects to maintain 3D effect (movement box at feet, hitbox in center of mass)
data CollisionGroup
    = Movement
    | MovementSensor
    | FloorEffect
    | Interact
    | Hitbox
    | Attack
    deriving (Show, Eq, Enum)

instance FromJSON CollisionGroup where
    parseJSON (String "movement")        = pure Movement
    parseJSON (String "movement-sensor") = pure MovementSensor
    parseJSON (String "floor-effect")    = pure FloorEffect
    parseJSON (String "interact")        = pure Interact
    parseJSON (String "hitbox")          = pure Hitbox
    parseJSON (String "attack")          = pure Attack
    parseJSON (String s)                 = fail $ "Unknown CollisionGroup " ++ show s
    parseJSON e                          = typeMismatch "CollisionGroup" e

mkGroupSensor :: CollisionGroup -> Sensor
mkGroupSensor Movement = Sensor False -- only Movement blocks; everything else is just used to mark hitting, etc.
mkGroupSensor _        = Sensor True

collisionGroups :: Vector CollisionFilter
collisionGroups = Vec.fromList
    [ mkFilter Movement       [Movement, MovementSensor, FloorEffect]
    , mkFilter MovementSensor [Movement]
    , mkFilter FloorEffect    [Movement]
    , mkFilter Interact       [Movement]
    , mkFilter Hitbox         [Attack]
    , mkFilter Attack         [Hitbox]
    ]
    where mkFilter g = CollisionFilter (fromIntegral . fromEnum $ g) (maskList [fromEnum g]) . maskList . map fromEnum

groupCollisionFilter :: CollisionGroup -> CollisionFilter
groupCollisionFilter = (collisionGroups Vec.!) . fromEnum

movementFilter, movementSensorFilter, floorEffectFilter, interactFilter, hitboxFilter, attackFilter :: CollisionFilter
movementFilter       = groupCollisionFilter Movement
movementSensorFilter = groupCollisionFilter MovementSensor
floorEffectFilter    = groupCollisionFilter FloorEffect
interactFilter       = groupCollisionFilter Interact
hitboxFilter         = groupCollisionFilter Hitbox
attackFilter         = groupCollisionFilter Attack
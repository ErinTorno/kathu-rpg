{-# LANGUAGE OverloadedStrings #-}

module Kathu.Entity.Physics.CollisionGroup where

import Apecs.Physics (maskList, CollisionFilter(..), Sensor(..))
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

-- movement is kept separate from hitboxes for all objects to maintain 3D effect (movement box at feet, hitbox in center of mass)
data CollisionGroup
    = Movement
    | FloorEffect
    | Interact
    | PlayerHitbox
    | EnemyHitbox
    | AllHitbox
    | PlayerAttack
    | EnemyAttack
    | AllAttack
      deriving (Show, Eq, Enum)

instance FromJSON CollisionGroup where
    parseJSON (String "movement")      = pure Movement
    parseJSON (String "floor-effect")  = pure FloorEffect
    parseJSON (String "interact")      = pure Interact
    parseJSON (String "player-hitbox") = pure PlayerHitbox
    parseJSON (String "enemy-hitbox")  = pure EnemyHitbox
    parseJSON (String "all-hitbox")    = pure AllHitbox
    parseJSON (String "player-attack") = pure PlayerAttack
    parseJSON (String "enemy-attack")  = pure EnemyAttack
    parseJSON (String "all-attack")    = pure AllAttack
    parseJSON e                        = typeMismatch "CollisionGroup" e

mkGroupSensor :: CollisionGroup -> Sensor
mkGroupSensor Movement = Sensor False -- only Movement blocks; everything else is just used to mark hitting, etc.
mkGroupSensor _        = Sensor True

groupCollisionFilter :: CollisionGroup -> CollisionFilter
groupCollisionFilter group =
    let cat      = (+1) . fromEnum -- we add 1 to the enum so that 0 will match with bit 1, etc.
        mkFilter = CollisionFilter (fromIntegral . cat $ group) (maskList [cat group]) . maskList . map cat
     in case group of
        Movement     -> mkFilter [Movement, FloorEffect]
        FloorEffect  -> mkFilter [Movement]
        Interact     -> mkFilter [Movement]
        PlayerHitbox -> mkFilter [EnemyAttack, AllAttack]
        EnemyHitbox  -> mkFilter [PlayerHitbox, AllAttack]
        AllHitbox    -> mkFilter [EnemyAttack, PlayerAttack, AllAttack]
        PlayerAttack -> mkFilter [EnemyHitbox, AllHitbox]
        EnemyAttack  -> mkFilter [PlayerHitbox, AllHitbox]
        AllAttack    -> mkFilter [PlayerHitbox, EnemyHitbox, AllHitbox]
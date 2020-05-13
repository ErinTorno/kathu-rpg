module Kathu.Entity.Physics.CollisionGroup
    ( CollisionGroup(..)
    , collisionGroupFromText
    , mkGroupSensor
    , groupCollisionFilter
    , collisionGroupDebugColor
    , collisionFilterDebugColor
    , movementFilter
    , movementSensorFilter
    , floorEffectFilter
    , interactFilter
    , hitboxFilter
    , attackFilter
    ) where

import           Apecs.Physics        (maskList, CollisionFilter(..), Sensor(..))
import           Data.Aeson
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec

import           Kathu.Graphics.Color

-- movement is kept separate from hitboxes for all objects to maintain 3D effect (movement box at feet, hitbox in center of mass)
data CollisionGroup
    = Movement
    | MovementSensor
    | FloorEffect
    | Interact
    | Hitbox
    | Attack
    | Intangible
    | EditorInfo -- Used by external editor to keep track of places object boundaries, etc.
    deriving (Show, Eq, Enum)

collisionGroupFromText :: Text -> Maybe CollisionGroup
collisionGroupFromText s = case s of
    "movement"        -> Just Movement
    "movement-sensor" -> Just MovementSensor
    "floor-effect"    -> Just FloorEffect
    "interact"        -> Just Interact
    "hitbox"          -> Just Hitbox
    "attack"          -> Just Attack
    "intangible"      -> Just Intangible
    _                 -> Nothing

instance FromJSON CollisionGroup where
    parseJSON = withText "CollisionGroup" $ \s -> case collisionGroupFromText s of
        Just g  -> pure g
        Nothing -> fail $ "Unknown CollisionGroup " ++ show s

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
    , mkFilter Intangible     ([] :: [CollisionGroup])
    , mkFilter EditorInfo     ([] :: [CollisionGroup])
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

-- | Returns a unique color for drawing collisions of this group
collisionGroupDebugColor :: CollisionGroup -> Color
collisionGroupDebugColor group = case group of
    Movement       -> mkColor  99 133 223 255
    MovementSensor -> mkColor  55  81 150 255
    FloorEffect    -> mkColor 133  82 156 255
    Interact       -> mkColor  88 204  79 255
    Hitbox         -> mkColor 223  58 108 255
    Attack         -> mkColor 223 143  58 255
    Intangible     -> mkColor  56  81  98 255
    EditorInfo     -> mkColor   0   0   0 255

collisionFilterDebugColor :: CollisionFilter -> Color
collisionFilterDebugColor = collisionGroupDebugColor . toEnum . fromIntegral . filterGroup
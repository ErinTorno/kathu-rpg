module Kathu.Entity.LifeTime where

import Apecs
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Int
import Data.Scientific  (toRealFloat)
import Data.Word

import Kathu.Entity.Components (CacheSize)

data LifeTime
    = LifeTimeGroup {-# UNPACK #-} !Word32
    | LifeTimeTimer {-# UNPACK #-} !Int32
    | Persistant -- Special state that marks this object as never being automatically deleted in cases such as world change
      deriving (Show, Eq)

instance Component LifeTime where type Storage LifeTime = Cache CacheSize (Map LifeTime)

instance FromJSON LifeTime where
    -- we only support grabbing persistant and timer-related LifeTimes from JSON, as the groups are only known at runtime
    parseJSON (String "persistant") = pure Persistant
    parseJSON (Number n)            = pure $ LifeTimeTimer (floor (toRealFloat n :: Double))
    parseJSON e                     = typeMismatch "LifeTime" e

updateLifeTime :: Word32 -> LifeTime -> LifeTime
updateLifeTime dT (LifeTimeTimer t) = LifeTimeTimer $ t - fromIntegral dT
updateLifeTime _ l = l

hasExpired :: LifeTime -> Bool
hasExpired (LifeTimeTimer t) | t <= 0    = True
                             | otherwise = False
hasExpired _ = False
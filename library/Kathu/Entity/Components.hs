{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We provide Component instances for some external types so that the configuration for them is all located here

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- Required for Apecs
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Entity.Components where

import Apecs
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Language.Haskell.TH.Syntax (Name)

import Kathu.Entity.Action
import Kathu.Entity.ActorState
import Kathu.Entity.Item (Inventory)
import Kathu.Entity.Physics.Floor (WorldFloor)
import Kathu.Graphics.Drawable (Render)
import Kathu.Graphics.Camera
import Kathu.Util.Types (Identifier(..))

type CacheSize = 2048

-- Component types and instances

data Identity = Identity
    { identifier :: Identifier
    , name :: Text
    , description :: Text
    } deriving (Show, Eq, Generic, ToJSON)
instance Component Identity where type Storage Identity = Cache CacheSize (Map Identity)

instance FromJSON Identity where
    parseJSON (String s) = pure $ Identity (Identifier s) "" "" -- basic one with only an id
    parseJSON (Object v) = Identity <$> v .: "id" <*> v .:? "name" .!= "" <*> v .:? "description" .!= ""
    parseJSON e          = typeMismatch "Identity" e

data LifeTime = LifeTimeGroup {-# UNPACK #-} !Word32 | LifeTimeTimer {-# UNPACK #-} !Word32 deriving (Show, Eq, Generic)
instance Component LifeTime where type Storage LifeTime = Cache CacheSize (Map LifeTime)

instance FromJSON LifeTime where
    -- we only support grabbing timer-related LifeTimes from JSON, as the groups are only known at runtime
    parseJSON (Number n) = pure $ LifeTimeTimer (floor (toRealFloat n :: Double))
    parseJSON e          = typeMismatch "LifeTime" e

updateLifeTime :: Word32 -> LifeTime -> LifeTime
updateLifeTime dT (LifeTimeTimer t) = LifeTimeTimer $ t - dT
updateLifeTime _ l = l

hasExpired :: LifeTime -> Bool
hasExpired (LifeTimeTimer t) | t <= 0    = True
                             | otherwise = False
hasExpired _ = False

newtype MovingSpeed = MovingSpeed Double deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component MovingSpeed where type Storage MovingSpeed = Map MovingSpeed

newtype Tags = Tags [Text] deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component Tags where type Storage Tags = Map Tags

-- Uniques

data Local = Local {actionPressed :: ActionPressed}
instance Component Local where type Storage Local = Unique Local

instance Component Camera where type Storage Camera = Unique Camera

----------------------------------
-- For external component types --
----------------------------------

instance Component ActorState where type Storage ActorState = Map ActorState

instance Component ActionSet where type Storage ActionSet = Map ActionSet

instance Component WorldFloor where type Storage WorldFloor = Cache CacheSize (Map WorldFloor)

-- This class defines how different components will get serialized and deserialized

data SerializableComponent = SerializableComponent {compName :: Name, requiresDependencies :: Bool, params :: [String]}

serializableComponentConfigs :: [SerializableComponent]
serializableComponentConfigs =
    [ SerializableComponent {compName = ''Identity,    requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''LifeTime,    requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''Tags,        requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''MovingSpeed, requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''Render,      requiresDependencies = True,  params = ["g"]}
    , SerializableComponent {compName = ''ActorState,  requiresDependencies = True,  params = []}
    , SerializableComponent {compName = ''Inventory,   requiresDependencies = True,  params = ["g"]}
    ]
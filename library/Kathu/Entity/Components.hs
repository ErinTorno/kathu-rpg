{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We provide Component instances for some external types so that the configuration for them is all located here

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- Required for Apecs
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Entity.Components where

import Apecs
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Linear.V2 (V2(..))
import GHC.Generics
import Language.Haskell.TH.Syntax (Name)

import Kathu.Entity.Action
import Kathu.Entity.ActorState
import Kathu.Entity.Item (Inventory)
import Kathu.Graphics.Drawable (Render)
import Kathu.Graphics.Camera
import Kathu.Util.Types (Identifier(..))

-- used for caches, which require Nat instead of a value
#define CATCH_SIZE 2048

-- Component types and instances

data Identity = Identity
    { identifier :: Identifier
    , name :: Text
    , description :: Text
    } deriving (Show, Eq, Generic, ToJSON)
instance Component Identity where type Storage Identity = Cache CATCH_SIZE (Map Identity)

instance FromJSON Identity where
    parseJSON (String s) = pure $ Identity (Identifier s) "" "" -- basic one with only an id
    parseJSON (Object v) = Identity <$> v .: "id" <*> v .:? "name" .!= "" <*> v .:? "description" .!= ""
    parseJSON e          = typeMismatch "Identity" e

-- different layers are treated as different world spaces that still use (relative) 2d coordinates
newtype Position = Position (V2 Float) deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component Position where type Storage Position = Cache CATCH_SIZE (Map Position)

-- should be replaced with a physics body in the future
newtype Velocity = Velocity (V2 Float) deriving (Show, Eq, Generic, ToJSON)
instance Component Velocity where type Storage Velocity = Cache CATCH_SIZE (Map Velocity)

instance FromJSON Velocity where
    -- a generic value for serialized entities to specify that they can move (and so have a velocity), but it is not set yet
    parseJSON (String "available") = pure . Velocity $ V2 0 0
    parseJSON v = genericParseJSON defaultOptions v

newtype MovingSpeed = MovingSpeed Float deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component MovingSpeed where type Storage MovingSpeed = Map MovingSpeed

newtype Tags = Tags [Text] deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component Tags where type Storage Tags = Map Tags

----------------------------------
-- For external component types --
----------------------------------

instance Component ActorState where type Storage ActorState = Map ActorState

instance Component ActionSet where type Storage ActionSet = Map ActionSet

-- Uniques

data Local = Local {actionPressed :: ActionPressed}
instance Component Local where type Storage Local = Unique Local

instance Component Camera where type Storage Camera = Unique Camera

-- This class defines how different components will get serialized and deserialized

data SerializableComponent = SerializableComponent {compName :: Name, requiresDependencies :: Bool, params :: [String]}

serializableComponentConfigs :: [SerializableComponent]
serializableComponentConfigs =
    [ SerializableComponent {compName = ''Identity,    requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''Velocity,    requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''MovingSpeed, requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''Tags,        requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''Render,      requiresDependencies = True,  params = ["g"]}
    , SerializableComponent {compName = ''ActorState,  requiresDependencies = True,  params = []}
    , SerializableComponent {compName = ''Inventory,   requiresDependencies = True,  params = ["g"]}
    ]
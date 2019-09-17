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
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Text (Text)
import Linear.V3 (V3(..))
import GHC.Generics
import Language.Haskell.TH.Syntax (Name)

import Kathu.Entity.Action
import Kathu.Entity.ActorState
import Kathu.Graphics.Drawable
import Kathu.Graphics.Camera
import Kathu.Util.Dependency
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
newtype Position = Position (V3 Float) deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component Position where type Storage Position = Cache CATCH_SIZE (Map Position)

-- should be replaced with a physics body in the future
newtype Velocity = Velocity (V3 Float) deriving (Show, Eq, Generic, ToJSON)
instance Component Velocity where type Storage Velocity = Cache CATCH_SIZE (Map Velocity)

instance FromJSON Velocity where
    -- a generic value for serialized entities to specify that they can move (and so have a velocity), but it is not set yet
    parseJSON (String "available") = pure . Velocity $ V3 0 0 0
    parseJSON v = genericParseJSON defaultOptions v

newtype MovingSpeed = MovingSpeed Float deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component MovingSpeed where type Storage MovingSpeed = Map MovingSpeed

newtype Tags = Tags [Text] deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance Component Tags where type Storage Tags = Map Tags

newtype Render g = Render {unRender :: Vector (RenderSprite g)}

instance (FromJSON (Dependency s m (RenderSprite g)), Monad m) => FromJSON (Dependency s m (Render g)) where
    parseJSON obj@(Object _) = (\v -> v >>= pure . Render . Vec.singleton) <$> parseJSON obj
    parseJSON str@(String _) = (\v -> v >>= pure . Render . Vec.singleton) <$> parseJSON str
    parseJSON (Array a)      = toRender <$> Vec.foldM run (pure []) a
        where run acc cur = (\rn -> rn >>= \inner -> (inner:) <$> acc) <$> parseJSON cur
              toRender ls = Render <$> (Vec.fromList <$> ls)
    parseJSON e              = typeMismatch "Render" e

----------------------------------
-- For external component types --
----------------------------------

instance Component ActorState where type Storage ActorState = Map ActorState

instance Component ActionSet where type Storage ActionSet = Map ActionSet

--type Inventory' = Inventory ImageID
--instance Component Inventory' where type Storage Inventory' = Map Inventory'

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
    ]
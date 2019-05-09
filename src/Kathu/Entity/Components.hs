{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- Required for Apecs
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Entity.Components where

import Apecs
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Kathu.Entity.Action
import Kathu.Entity.ActorState
import Kathu.Entity.Item
import Kathu.Graphics.Drawable
import Kathu.Graphics.Camera
import Kathu.Physics.Body
import Linear.V3 (V3)
import Data.Vector (Vector)

-- used for caches, which require Nat instead of a value
#define CATCH_SIZE 1024

-- Component types and instances

data SpecialEntity = Player | ReplaceMeLater deriving (Show, Eq, Generic)
instance Component SpecialEntity where type Storage SpecialEntity = Map SpecialEntity

data Identity = Identity
    { identifier :: Text
    , name :: Text
    , description :: Text
    } deriving (Show, Eq, Generic)
instance Component Identity where type Storage Identity = Cache CATCH_SIZE (Map Identity)

-- different layers are treated as different world spaces that still use (relative) 2d coordinates
newtype Position = Position (V3 Float) deriving (Show, Eq, Generic)
instance Component Position where type Storage Position = Cache CATCH_SIZE (Map Position)

-- should be replaced with a physics body in the future
newtype Velocity = Velocity (V3 Float) deriving (Show, Eq, Generic)
instance Component Velocity where type Storage Velocity = Cache CATCH_SIZE (Map Velocity)

newtype MovingSpeed = MovingSpeed Float deriving (Show, Eq, Generic)
instance Component MovingSpeed where type Storage MovingSpeed = Map MovingSpeed

newtype Tags = Tags [Text] deriving (Show, Eq, Generic)
instance Component Tags where type Storage Tags = Map Tags

newtype Render = Render {unRender :: Vector RenderSprite}
instance Component Render where type Storage Render = Map Render

----------------------------------
-- For external component types --
----------------------------------

instance Component ActorState where type Storage ActorState = Map ActorState

instance Component ActionSet where type Storage ActionSet = Map ActionSet

-- the body stores an entity as its parent for use during physics calculations
type Body' = Body Entity
instance Component (Body a) where type Storage (Body a) = Cache CATCH_SIZE (Map (Body a))

instance Component Inventory where type Storage Inventory = Map Inventory

-- Uniques

data Local = Local {actionPressed :: ActionPressed}
instance Component Local where type Storage Local = Unique Local

instance Component Camera where type Storage Camera = Unique Camera

-- ECS Util
-- selects all unique and non-unique components that an individual entity might have
type AllComponents =
    ( (Identity, Position, Velocity, MovingSpeed, (Body Entity))
    , (Tags, Render, ActorState, Inventory)
    , (Render, Local, Camera)
    )

-- these components can be serialized from Strings without any monads
pureSerialComponents = [''SpecialEntity, ''Identity, ''Position, ''Velocity, ''MovingSpeed, ''Tags]
-- these require the SystemLink monad to be deserialized, and so they are kept separate
linkedSerialComponents = [''Render, ''Inventory, ''ActorState]
allSerialComponents = pureSerialComponents ++ linkedSerialComponents

generalComponents = allSerialComponents ++ [''ActionSet, ''Body']
allNonGlobal = generalComponents ++ [''Local, ''Camera]

-- Misc helper functions for working with these components

hasTag :: Text -> Tags -> Bool
hasTag t (Tags ls) = elem t ls

appendTag :: Text -> Tags -> Tags
appendTag t (Tags ls)= Tags $ t: ls

removeTag :: Text -> Tags -> Tags
removeTag t (Tags ls) = Tags $ filter (/=t) ls
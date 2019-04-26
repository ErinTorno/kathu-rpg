{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- Required for Apecs
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Entity.Components where

import Apecs
import qualified Data.Map as M
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Kathu.Entity.Action
import Kathu.Entity.Item
import qualified Kathu.Entity.Resource as R
import Kathu.Graphics.Drawable
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

newtype Render = Render {sprites :: Vector RenderSprite}
instance Component Render where type Storage Render = Map Render

newtype Team = Team (Int) deriving (Show, Eq, Generic)
data ActorState = ActorState
    { team    :: Team
    , health  :: R.Dynamic Float
    , mana    :: R.Dynamic Float
    , armor   :: R.Static Float
    , aura    :: R.Static Float
    , resists :: M.Map Text (R.Static Float)
    } deriving (Show, Eq, Generic)
instance Component ActorState where type Storage ActorState = Map ActorState

----------------------------------
-- For external component types --
----------------------------------

instance Component ActionSet where type Storage ActionSet = Map ActionSet

-- the body stores an entity as its parent for use during physics calculations
type Body' = Body Entity
instance Component (Body a) where type Storage (Body a) = Cache CATCH_SIZE (Map (Body a))

instance Component Inventory where type Storage Inventory = Map Inventory

-- Uniques

data Local = Local {actionPressed :: ActionPressed}
instance Component Local where type Storage Local = Unique Local

data Camera = Camera {width :: Word32, height :: Word32, zoom :: Float}
instance Component Camera where type Storage Camera = Unique Camera

-- ECS Util
-- selects all unique and non-unique components that an individual entity might have
type AllComponents =
    ( (Identity, Position, Velocity, MovingSpeed, (Body Entity))
    , (Tags, Render, Team, ActorState, Inventory)
    , (Render, Local, Camera)
    )

-- these components can be serialized from Strings without any monads
pureSerialComponents = [''SpecialEntity, ''Identity, ''Position, ''Velocity, ''MovingSpeed, ''Tags, ''ActorState]
-- these require the SystemLink monad to be deserialized, and so they are kept separate
linkedSerialComponents = [''Render, ''Inventory]
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
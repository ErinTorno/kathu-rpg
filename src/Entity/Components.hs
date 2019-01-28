{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- Required for Apecs
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Entity.Components where

import Apecs
import qualified Data.Map as M
import Data.Text (Text)
import Data.Word
import GHC.Generics
import SDL (V3)
import Data.Vector (Vector)

import Entity.Action
import qualified Entity.Resource as R
import Graphics.Drawable

-- Component types and instances

data SpecialEntity = Player | ReplaceMeLater deriving (Show, Eq, Generic)
instance Component SpecialEntity where type Storage SpecialEntity = Map SpecialEntity

data Identity = Identity
    { identifier :: Text
    , name :: Text
    , description :: Text
    } deriving (Show, Eq, Generic)
instance Component Identity where type Storage Identity = Cache 1024 (Map Identity)

-- Although appears as 2D, we keep the world in 3d to ease collision and multi-floor dungeons
newtype Position = Position (V3 Float) deriving (Show, Eq, Generic)
instance Component Position where type Storage Position = Cache 1024 (Map Position)

-- should be changed to physics body in the future
newtype Velocity = Velocity (V3 Float) deriving (Show, Eq, Generic)
instance Component Velocity where type Storage Velocity = Cache 1024 (Map Velocity)

newtype MovingSpeed = MovingSpeed Float deriving (Show, Eq, Generic)
instance Component MovingSpeed where type Storage MovingSpeed = Map MovingSpeed

newtype Tags = Tags [Text] deriving (Show, Eq, Generic)
instance Component Tags where type Storage Tags = Map Tags

newtype Render = Render {sprites :: Vector RenderSprite} deriving (Show, Eq)
instance Component Render where type Storage Render = Map Render

{-
data Render = Render {drawable :: Drawable, scale :: Float, rotation :: Float}
instance Component Render where type Storage Render = Map Render
-}

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

-- For external component types
instance Component ActionSet where type Storage ActionSet = Map ActionSet

-- Uniques

data Local = Local {actionPressed :: ActionPressed}
instance Component Local where type Storage Local = Unique Local

data Camera = Camera {width :: Word32, height :: Word32, zoom :: Float}
instance Component Camera where type Storage Camera = Unique Camera

-- ECS Util


{-
componentTypes = [''SpecialEntity, ''Identity, ''Position, ''Velocity, ''Moving, ''Tags, ''ActorState, {-Unserializable-} ''ActionSet, ''Render]
serializableTypes = [''SpecialEntity, ''Identity, ''Position, ''Velocity, ''Moving, ''Tags, ''ActorState]
-}

-- selects all unique and non-unique components that an individual entity might have
type AllComponents = (Position, Velocity, MovingSpeed, Tags, ActorState, ActionSet, Render, Local, Camera)

-- these components can be serialized from Strings without any monads
pureSerialComponents = [''SpecialEntity, ''Identity, ''Position, ''Velocity, ''MovingSpeed, ''Tags, ''ActorState]
-- these require the SystemLink monad to be deserialized, and so they are kept separate
linkedSerialComponents = [''Render]
allSerialComponents = pureSerialComponents ++ linkedSerialComponents

generalComponents = allSerialComponents ++ [''ActionSet]
allNonGlobal = generalComponents ++ [''Local, ''Camera]

-- Misc helper functions for working with these components

hasTag :: Text -> Tags -> Bool
hasTag t (Tags ls) = elem t ls

appendTag :: Text -> Tags -> Tags
appendTag t (Tags ls)= Tags $ t: ls

removeTag :: Text -> Tags -> Tags
removeTag t (Tags ls) = Tags $ filter (/=t) ls
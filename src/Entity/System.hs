{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- Required for Apecs
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Entity.System where

import Apecs
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Monoid
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import qualified System.Random as R

import Entity.Action
import Entity.Components
import Entity.Prototype
import IO.Settings

-- We create an entity prototype that supports all given component types
defineData          "EntityPrototype" "" allSerialComponents
defineEntityCreator "newFromPrototype" "" allSerialComponents

-- Globals

-- these are kept separate, as the times for each type of loop may become desynced
-- ex: We may update the physics multiple times to "catch-up" if there was a delay in the system
--     Or update the graphics multiple times to display higher frames while we wait for the physics to run again
newtype PhysicsTime = PhysicsTime (Word32) deriving (Show, Eq)
instance Semigroup PhysicsTime where (<>) = mappend
instance Monoid PhysicsTime where mempty = PhysicsTime 0
instance Component PhysicsTime where type Storage PhysicsTime = Global PhysicsTime

newtype RenderTime = RenderTime (Word32) deriving (Show, Eq)
instance Semigroup RenderTime where (<>) = mappend
instance Monoid RenderTime where mempty = RenderTime 0
instance Component RenderTime where type Storage RenderTime = Global RenderTime

newtype Random = Random (R.StdGen)
instance Semigroup Random where (<>) = mappend
instance Monoid Random where mempty = Random $ R.mkStdGen 0 -- the IO portion of this is expected to initialize it with a seed
instance Component Random where type Storage Random = Global Random

instance Semigroup Settings where (<>) = mappend
instance Monoid Settings where mempty = defaultSettings
instance Component Settings where type Storage Settings = Global Settings

-- | This data type plays the role as a collection of named values for the game to read from when loading a level
data Library = Library
    { prototypes :: M.Map Text EntityPrototype
    }

emptyLibrary = Library M.empty

instance Semigroup Library where (<>) = mappend
instance Monoid Library where mempty = emptyLibrary
instance Component Library where type Storage Library = Global Library

-- World

makeWorld "EntityWorld" $ allNonGlobal ++ [''PhysicsTime, ''RenderTime, ''Random, ''Settings, ''Library]

type System' a = System EntityWorld a
type SystemT' m a = SystemT EntityWorld m a

-- Entity functions

destroyEntity ety = destroy ety (Proxy @AllComponents)

stepPhysicsTime :: Word32 -> System' ()
stepPhysicsTime dT = modify global $ \(PhysicsTime t) -> PhysicsTime (t + dT)

stepRenderTime :: Word32 -> System' ()
stepRenderTime dT = modify global $ \(RenderTime t) -> RenderTime (t + dT)
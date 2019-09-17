{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
-- due to working with templates and complicated types related to Apecs
-- we sometimes can't give a type signature to functions like destroyEntity
-- without it becoming outdated too quickly
-- we also need orphan instances to set up the Apecs system

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.App.System where

import Apecs
import qualified Data.Map as Map
import Data.Semigroup (Semigroup)
import qualified Data.Vector as Vec

import Kathu.App.Data.Library
import Kathu.App.Data.Settings
import Kathu.App.Graphics.Image (ImageID)
import Kathu.App.Graphics.ImageManager
import Kathu.App.Graphics.UI
import Kathu.Entity.Action
import Kathu.Entity.ActorState
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Graphics.Camera
import Kathu.World.Time (WorldTime(..))
import Kathu.World.WorldSpace (WorldSpace, emptyWorldSpace)

type Render' = Render ImageID
instance Component Render' where type Storage Render' = Map Render'

-- ECS Util
-- selects all unique and non-unique components that an individual entity might have
type AllComponents =
    ( (Identity, Position, Velocity, MovingSpeed)
    , (Tags, Render', ActorState)
    , (Local, Camera)
    )
    
-- Globals

instance Semigroup Settings where (<>) = mappend
instance Monoid Settings where mempty = defaultSettings
instance Component Settings where type Storage Settings = Global Settings

instance Semigroup ImageManager where (<>) = mappend
instance Monoid ImageManager where mempty = defaultImageManager
instance Component ImageManager where type Storage ImageManager = Global ImageManager

instance Semigroup UIConfig where (<>) = mappend
instance Monoid UIConfig where mempty = error "Attempted to use UIConfig before it has been loaded"
instance Component UIConfig where type Storage UIConfig = Global UIConfig

type WorldSpace' = WorldSpace ImageID
instance Semigroup WorldSpace'  where (<>) = mappend
instance Monoid WorldSpace'  where mempty = emptyWorldSpace
instance Component WorldSpace'  where type Storage WorldSpace'  = Global WorldSpace' 

instance Semigroup Library where (<>) = mappend
instance Monoid Library where mempty = Library Vec.empty mempty Map.empty Map.empty Map.empty Map.empty
instance Component Library where type Storage Library = Global Library

-- World

makeWorld "EntityWorld"
    $ [''Identity, ''Velocity, ''MovingSpeed, ''Tags, ''Render', ''ActorState, ''Position, ''ActionSet, ''Local, ''Camera]
   ++ [''LogicTime, ''RenderTime, ''WorldTime, ''Random, ''Settings, ''ImageManager, ''UIConfig, ''WorldSpace', ''Library, ''Debug]

type System' a = System EntityWorld a
type SystemT' m a = SystemT EntityWorld m a

-- Entity functions

destroyEntity ety = destroy ety (Proxy @AllComponents)
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we also need orphan instances to set up the Apecs system

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.App.System where

import           Apecs
import           Apecs.Physics
import           Control.Monad.IO.Class          (MonadIO)
import           Data.Semigroup                  (Semigroup)

import           Kathu.App.Data.Library
import           Kathu.App.Data.Settings
import           Kathu.App.Graphics.Font
import           Kathu.App.Graphics.Image        (ImageID)
import           Kathu.App.Graphics.ImageManager
import           Kathu.App.Graphics.UI
import           Kathu.Entity.Action
import           Kathu.Entity.ActorState
import           Kathu.Entity.Components
import           Kathu.Entity.Item               (Inventory)
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.BodyConfig (setBodyConfig)
import           Kathu.Entity.Physics.Floor      (WorldFloor)
import           Kathu.Entity.Prototype
import           Kathu.Entity.System
import           Kathu.Entity.Time
import qualified Kathu.Scripting.Lua             as Lua
import           Kathu.Scripting.Lua.Types       (ActiveScript, ScriptBank)
import           Kathu.Scripting.Variables       (Variables)
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Drawable         (Render)
import           Kathu.Graphics.Palette          (PaletteManager)
import           Kathu.Util.Apecs
import           Kathu.World.Stasis              (WorldStases)
import           Kathu.World.Time                (WorldTime)
import           Kathu.World.WorldSpace          (WorldSpace, emptyWorldSpace)

type Inventory' = Inventory ImageID
instance Component Inventory' where type Storage Inventory' = Map Inventory'

type Render' = Render ImageID
instance Component Render' where type Storage Render' = Map Render'

-- ECS Util
-- selects all unique and non-unique components that an individual entity might have
type AllComponents =
    ( Existance
    , ActiveScript
    , (Identity, LifeTime, WorldFloor, Tags, Render', Body)
    , (MovingSpeed, ActorState, Inventory', ActionSet)
    , (Local, Camera)
    )
    
-- New Globals

type Tiles' = Tiles ImageID
instance Semigroup Tiles' where (<>) = mappend
instance Monoid Tiles' where mempty  = error "Attempted to use Tiles before it has been loaded"
instance Component Tiles' where type Storage Tiles' = Global Tiles'

instance Semigroup Settings where (<>) = mappend
instance Monoid Settings where mempty = defaultSettings
instance Component Settings where type Storage Settings = Global Settings

instance Semigroup ImageManager where (<>) = mappend
instance Monoid ImageManager where mempty = defaultImageManager
instance Component ImageManager where type Storage ImageManager = Global ImageManager

instance Semigroup FontCache where (<>) = mappend
instance Monoid FontCache where mempty = error "Attempted to use FontCache before it has been loaded"
instance Component FontCache where type Storage FontCache = Global FontCache

instance Semigroup UIConfig where (<>) = mappend
instance Monoid UIConfig where mempty = error "Attempted to use UIConfig before it has been loaded"
instance Component UIConfig where type Storage UIConfig = Global UIConfig

type WorldSpace' = WorldSpace ImageID
instance Semigroup WorldSpace'  where (<>) = mappend
instance Monoid WorldSpace'  where mempty = emptyWorldSpace
instance Component WorldSpace'  where type Storage WorldSpace'  = Global WorldSpace'

instance Semigroup Library where (<>) = mappend
instance Monoid Library where mempty = emptyLibrary
instance Component Library where type Storage Library = Global Library

instance Semigroup ScriptBank where (<>) = mappend
instance Monoid ScriptBank where mempty = error "Attempted to use ScriptBank before it has been loaded"
instance Component ScriptBank where type Storage ScriptBank = Global ScriptBank

-- World

makeWorld "EntityWorld"
    $ [''Physics]
   ++ [''Existance, ''Identity, ''LifeTime, ''ActiveScript, ''WorldFloor, ''MovingSpeed, ''Tags, ''Render', ''ActorState, ''Inventory', ''ActionSet, ''Local, ''Camera]
   ++ [''LogicTime, ''RenderTime, ''WorldTime, ''PaletteManager, ''Random, ''WorldStases, ''FloorProperties, ''Tiles', ''Variables, ''Debug]
   ++ [''Settings, ''ImageManager, ''FontCache, ''UIConfig, ''WorldSpace', ''Library, ''ScriptBank]

type System' a = System EntityWorld a
type SystemT' m a = SystemT EntityWorld m a

-- Entity functions

externalFunctions :: Lua.ExternalFunctions EntityWorld ImageID
externalFunctions = Lua.ExternalFunctions
    { Lua.setPalette         = setPaletteManager
    , Lua.getEntityPrototype = error "getEntityPrototype not implemented"
    , Lua.newFromPrototype   = newFromPrototype
    , Lua.destroyEntity      = error "destroyEntity not implemented" -- destroyEntity -- don't use yet, game halts with "<<loop>>" with just this
    }

destroyEntity :: MonadIO m => Entity -> SystemT' m ()
destroyEntity ety = do
    whenExists ety $ \active -> liftIO (Lua.releaseActiveScript active)
    
    destroy ety (Proxy @AllComponents)

newFromPrototype :: EntityPrototype ImageID -> SystemT' IO Entity
newFromPrototype proto = do
    ety <- newFromSimplePrototype proto

    setBodyConfig ety . bodyConfig $ proto
    case script proto of
        Nothing    -> pure ()
        (Just scr) -> do
            activeScr <- Lua.loadScript externalFunctions scr
            ety $= activeScr

    pure ety
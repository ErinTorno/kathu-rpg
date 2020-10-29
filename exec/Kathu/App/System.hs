{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we also need orphan instances to set up the Apecs system

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.App.System where

import           Apecs
import           Apecs.Physics
import           Control.Monad                   (forM_, void)
import           Verda.Graphics.SpriteManager    (setPaletteManager)
import           Verda.System.Tile.Components    (Chunks)
import           Verda.Util.Apecs
import           Verda.World                     (DeletableBaseVerdaComponents, Existance(..), baseVerdaComponentNames)

import           Kathu.App.Tools.ToolMode
import           Kathu.Config.Dictionary         (Dictionary, emptyDictionary)
import           Kathu.Config.Settings
import           Kathu.Entity.Action
import           Kathu.Entity.ActorState
import           Kathu.Entity.Components
import           Kathu.Entity.Item               (Inventory)
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.BodyConfig (setBodyConfig)
import           Kathu.Entity.Physics.Floor      (WorldFloor)
import           Kathu.Entity.Prefab
import           Kathu.Entity.System
import           Kathu.Graphics.Palette          (PaletteManager)
import           Kathu.Graphics.UI
import           Kathu.Language                  (Language)
import qualified Kathu.Scripting.Lua             as Lua
import           Kathu.Scripting.Lua.Types       (ActiveScript, RunningScriptEntity(..), ScriptBank, ScriptEventBuffer(..))
import           Kathu.Scripting.Variables       (Variables)
import           Kathu.Scripting.Wire
import           Kathu.World.Stasis              (WorldStases)
import           Kathu.World.Tile                (AllTiles')
import           Kathu.World.Time                (WorldTime)
import           Kathu.World.WorldSpace          (EditorInstancedFromWorld, WorldSpace)

-- ECS Util
-- selects all unique and non-unique components that an individual entity might have
type AllComponents =
    ( (ActiveScript, SpecialEntity)
    , (Identity, LifeTime, WorldFloor, Tags, Body)
    , (MovingSpeed, ActorState, Inventory, ActionSet, Local)
    , (Body, Shape, Constraint)
    , DeletableBaseVerdaComponents
    )
    
-- New Globals

instance Semigroup Settings where (<>) = mappend
instance Monoid Settings where mempty = defaultSettings
instance Component Settings where type Storage Settings = Global Settings

instance Semigroup UIConfig where (<>) = mappend
instance Monoid UIConfig where mempty = error "Attempted to use UIConfig before it has been loaded"
instance Component UIConfig where type Storage UIConfig = Global UIConfig

instance Semigroup Dictionary where (<>) = mappend
instance Monoid Dictionary where mempty = emptyDictionary
instance Component Dictionary where type Storage Dictionary = Global Dictionary

instance Semigroup ScriptBank where (<>) = mappend
instance Monoid ScriptBank where mempty = error "Attempted to use ScriptBank before it has been loaded"
instance Component ScriptBank where type Storage ScriptBank = Global ScriptBank

instance Semigroup RunningScriptEntity where (<>) = mappend
instance Monoid RunningScriptEntity where mempty = RunningScriptEntity Nothing
instance Component RunningScriptEntity where type Storage RunningScriptEntity = Global RunningScriptEntity

instance Semigroup ScriptEventBuffer where (<>) = mappend
instance Monoid ScriptEventBuffer where mempty = ScriptEventBuffer []
instance Component ScriptEventBuffer where type Storage ScriptEventBuffer = Global ScriptEventBuffer

instance Semigroup WireReceivers where (<>) = mappend
instance Monoid WireReceivers where mempty = error "Attempted to use WireReceivers before it has been loaded"
instance Component WireReceivers where type Storage WireReceivers = Global WireReceivers

-- World

makeWorld "EntityWorld"
    $ [''Physics]
   ++ baseVerdaComponentNames
   ++ [''SpecialEntity, ''Identity, ''LifeTime, ''ActiveScript, ''WorldFloor, ''MovingSpeed, ''Tags, ''ActorState, ''Inventory, ''EditorInstancedFromWorld, ''ActionSet]
   ++ [''Local, ''Player]
   ++ [''AllTiles', ''Chunks, ''WorldTime, ''PaletteManager, ''Random, ''WorldStases, ''FloorProperties, ''Variables, ''IncludeEditorInfo]
   ++ [''Settings, ''UIConfig, ''WorldSpace, ''Dictionary, ''Language, ''ScriptBank, ''RunningScriptEntity, ''ScriptEventBuffer, ''WireReceivers]
   ++ [''ToolMode, ''ToolModeUniversalState]

type System' a = System EntityWorld a
type SystemT' m a = SystemT EntityWorld m a

-- Entity functions
-- TODO Move into Prefab module

externalFunctions :: Lua.ExternalFunctions EntityWorld
externalFunctions = Lua.ExternalFunctions
    { Lua.setPalette      = setPaletteManager
    , Lua.getEntityPrefab = error "getEntityPrototype not implemented"
    , Lua.newFromPrefab   = newFromPrefab
    }

destroyEntity :: Entity -> SystemT' IO ()
destroyEntity ety = do
    whenExists ety Lua.releaseActiveScript
    
    destroy ety (Proxy @AllComponents)

newFromPrefabWithScriptMapping :: (ActiveScript -> ActiveScript) -> Prefab -> SystemT' IO Entity
newFromPrefabWithScriptMapping f Prefab{..} = do
    ety <- newEntity (Existance, pIdentity)
    forM_ pActorState    (ety$=)
    forM_ pInventory     (ety$=)
    forM_ pLifeTime      (ety$=)
    forM_ pMovingSpeed   (ety$=)
    forM_ pSprite        (ety$=)
    forM_ pSpecialEntity (ety$=)
    forM_ pTags          (ety$=)
    forM_ pScript        (void . Lua.loadScript f externalFunctions ety)
    setBodyConfig ety pBodyConfig
    return ety

newFromPrefab :: Prefab -> SystemT' IO Entity
newFromPrefab = newFromPrefabWithScriptMapping id
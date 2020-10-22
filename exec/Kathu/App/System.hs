{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we also need orphan instances to set up the Apecs system

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.App.System where

import           Apecs
import           Apecs.Physics
import           Control.Monad                   (forM_, void)
import           Verda.Event.Controls
import           Verda.Graphics.Components
import           Verda.Graphics.SpriteManager    (setPaletteManager)
import           Verda.Logger                    (Logger)
import           Verda.Time
import           Verda.Util.Apecs

import           Kathu.App.Data.Dictionary       (Dictionary, emptyDictionary)
import           Kathu.App.Data.Settings
import           Kathu.App.Graphics.UI
import           Kathu.App.Tools.ToolMode
import           Kathu.Entity.Action
import           Kathu.Entity.ActorState
import           Kathu.Entity.Components
import           Kathu.Entity.Item               (Inventory)
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.BodyConfig (setBodyConfig)
import           Kathu.Entity.Physics.Floor      (WorldFloor)
import           Kathu.Entity.Prefab
import           Kathu.Entity.System
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Drawable         (Render)
import           Kathu.Graphics.Palette          (PaletteManager)
import           Kathu.Language                  (Language)
import qualified Kathu.Scripting.Lua             as Lua
import           Kathu.Scripting.Lua.Types       (ActiveScript, RunningScriptEntity(..), ScriptBank, ScriptEventBuffer(..))
import           Kathu.Scripting.Variables       (Variables)
import           Kathu.Scripting.Wire
import           Kathu.World.Stasis              (WorldStases)
import           Kathu.World.Time                (WorldTime)
import           Kathu.World.WorldSpace          (EditorInstancedFromWorld, WorldSpace, emptyWorldSpace)

type Inventory' = Inventory SpriteID
instance Component Inventory' where type Storage Inventory' = Map Inventory'

type Render' = Render SpriteID
instance Component Render' where type Storage Render' = Map Render'

-- ECS Util
-- selects all unique and non-unique components that an individual entity might have
type AllComponents =
    ( Existance
    , (ActiveScript, SpecialEntity)
    , (Identity, LifeTime, WorldFloor, Tags, Render', Body)
    , (MovingSpeed, ActorState, Inventory', ActionSet)
    , (Local, Camera)
    , (Body, Shape, Constraint)
    )
    
-- New Globals

newtype ShouldQuit = ShouldQuit Bool
instance Semigroup ShouldQuit where (<>) = mappend
instance Monoid ShouldQuit where mempty = ShouldQuit False
instance Component ShouldQuit where type Storage ShouldQuit = Global ShouldQuit

type Tiles' = Tiles SpriteID
instance Semigroup Tiles' where (<>) = mappend
instance Monoid Tiles' where mempty  = error "Attempted to use Tiles before it has been loaded"
instance Component Tiles' where type Storage Tiles' = Global Tiles'

instance Semigroup Settings where (<>) = mappend
instance Monoid Settings where mempty = defaultSettings
instance Component Settings where type Storage Settings = Global Settings

instance Semigroup UIConfig where (<>) = mappend
instance Monoid UIConfig where mempty = error "Attempted to use UIConfig before it has been loaded"
instance Component UIConfig where type Storage UIConfig = Global UIConfig

type WorldSpace' = WorldSpace SpriteID
instance Semigroup WorldSpace'  where (<>) = mappend
instance Monoid WorldSpace'  where mempty = emptyWorldSpace
instance Component WorldSpace'  where type Storage WorldSpace'  = Global WorldSpace'

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
   ++ [''Existance, ''SpecialEntity, ''Identity, ''LifeTime, ''ActiveScript, ''WorldFloor, ''MovingSpeed, ''Tags, ''Render', ''ActorState, ''Inventory', ''EditorInstancedFromWorld, ''ActionSet]
   ++ [''Local, ''Camera, ''Player]
   ++ [''ShouldQuit, ''LogicTime, ''RenderTime, ''WorldTime, ''PaletteManager, ''Random, ''WorldStases, ''FloorProperties, ''Tiles', ''Variables, ''Debug, ''IncludeEditorInfo, ''Logger]
   ++ [''Settings, ''CursorMotionState, ''ControlState, ''FontCache, ''UIConfig, ''WorldSpace', ''Dictionary, ''ScriptBank, ''RunningScriptEntity, ''ScriptEventBuffer, ''WireReceivers]
   ++ [''BackgroundColor, ''Language, ''SpriteManager]
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
    , Lua.destroyEntity   = error "destroyEntity not implemented" -- destroyEntity -- don't use yet, game halts with "<<loop>>" with just this
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
    forM_ pRender        (ety$=)
    forM_ pSpecialEntity (ety$=)
    forM_ pTags          (ety$=)
    forM_ pScript        (void . Lua.loadScript f externalFunctions ety)
    setBodyConfig ety pBodyConfig
    return ety

newFromPrefab :: Prefab -> SystemT' IO Entity
newFromPrefab = newFromPrefabWithScriptMapping id
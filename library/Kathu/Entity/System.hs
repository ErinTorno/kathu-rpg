{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.System where

import           Apecs
import           Apecs.Physics
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (forM_, void)
import qualified Data.Map                        as Map
import           Verda.Graphics.SpriteManager    (setPaletteManager)
import           Verda.System.Tile.Components    (Chunks)
import           Verda.Util.Apecs
import           Verda.Util.Types
import           Verda.World                     (DeletableBaseVerdaComponents, Existance(..), baseVerdaComponentNames)

import           Kathu.Config.Dictionary         (Dictionary, dictPrefabs)
import           Kathu.Config.Settings
import           Kathu.Editor.Tools.Info
import           Kathu.Editor.Tools.ToolMode
import           Kathu.Entity.Action
import           Kathu.Entity.ActorState
import           Kathu.Entity.Components
import           Kathu.Entity.Item               (Inventory)
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.BodyConfig (setBodyConfig)
import           Kathu.Entity.Physics.Floor      (FloorProperties, WorldFloor)
import           Kathu.Entity.Prefab
import           Kathu.Graphics.Palette          (PaletteManager)
import           Kathu.Graphics.UI
import           Kathu.Language                  (Language)
import           Kathu.Random
import qualified Kathu.Scripting.Lua             as Lua
import           Kathu.Scripting.Lua.Types       (ActiveScript, LuaModules, RunningScriptEntity(..), ScriptBank, ScriptEventBuffer(..))
import           Kathu.Scripting.Variables       (Variables)
import           Kathu.Scripting.Wire
import           Kathu.World.Stasis              (WorldStases)
import           Kathu.World.Tile                (AllTiles')
import           Kathu.World.Time                (WorldTime)
import           Kathu.World.WorldSpace          (EditorInstancedFromWorld, WorldInventory, WorldSpace)

-- ECS Util
-- selects all unique and non-unique components that an individual entity might have
type AllComponents =
    ( (ActiveScript, SpecialEntity)
    , (Identity, LifeTime, WorldFloor, Tags, Body)
    , (MovingSpeed, ActorState, Inventory, ActionSet, Local)
    , (Body, Shape, Constraint)
    , DeletableBaseVerdaComponents
    )

-- World

makeWorld "KathuWorld"
    $ [''Physics]
   ++ baseVerdaComponentNames
   ++ [''SpecialEntity, ''Identity, ''LifeTime, ''ActiveScript, ''WorldFloor, ''MovingSpeed, ''Tags, ''ActorState, ''Inventory, ''EditorInstancedFromWorld, ''ActionSet]
   ++ [''Local, ''Player]
   ++ [''Dictionary, ''Language, ''PaletteManager, ''Random, ''Settings, ''UIConfig]
   ++ [''LuaModules, ''RunningScriptEntity, ''ScriptBank, ''ScriptEventBuffer, ''Variables, ''WireReceivers]
   ++ [''AllTiles', ''Chunks, ''FloorProperties, ''WorldTime, ''WorldInventory, ''WorldSpace, ''WorldStases]
   ++ [''IncludeEditorInfo, ''ToolMode, ''ToolModeUniversalState]

type System' a = System KathuWorld a
type SystemT' m a = SystemT KathuWorld m a

----------------------
-- Entity Functions --
----------------------

-- TODO move this to Graphics when palettes are re-implemented to work with Verda
setPalette :: Identifier -> SystemT' IO Bool
setPalette = setPaletteManager

destroyEntity :: Entity -> SystemT' IO ()
destroyEntity ety = do
    whenExists ety Lua.releaseActiveScript
    destroy ety (Proxy @AllComponents)

newFromPrefabWithScriptMapping :: (Lua.ActiveScript -> Lua.ActiveScript) -> Prefab -> SystemT' IO Entity
newFromPrefabWithScriptMapping f Prefab{..} = do
    ety <- newEntity (Existance, pIdentity)
    forM_ pActorState    (ety$=)
    forM_ pInventory     (ety$=)
    forM_ pLifeTime      (ety$=)
    forM_ pMovingSpeed   (ety$=)
    forM_ pSprite        (ety$=)
    forM_ pSpecialEntity (ety$=)
    forM_ pTags          (ety$=)
    forM_ pScript        (void . Lua.loadScript f ety)
    setBodyConfig ety pBodyConfig
    return ety

newFromPrefab :: Prefab -> SystemT' IO Entity
newFromPrefab = newFromPrefabWithScriptMapping id

lookupEntityPrefab :: Identifier -> SystemT' IO (Maybe Prefab)
lookupEntityPrefab idt = Map.lookup idt . (^.dictPrefabs) <$> get global
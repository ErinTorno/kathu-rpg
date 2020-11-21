{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.System where

import           Apecs
import           Apecs.Physics
import           Verda.System.Tile.Components    (Chunks)
import           Verda.World                     (DeletableBaseVerdaComponents, baseVerdaComponentNames)

import           Kathu.Config.Dictionary         (Dictionary)
import           Kathu.Config.Settings
import           Kathu.Editor.Tools.Info
import           Kathu.Editor.Tools.ToolMode
import           Kathu.Entity.Action
import           Kathu.Entity.ActorState
import           Kathu.Entity.Components
import           Kathu.Entity.Item               (Inventory)
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.Floor      (FloorProperties, WorldFloor)
import           Kathu.Graphics.Palette          (PaletteManager)
import           Kathu.Graphics.UI
import           Kathu.Language                  (Language)
import           Kathu.Random
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

-- World

makeWorld "KathuWorld"
    $ [''Physics]
   ++ baseVerdaComponentNames
   ++ [''SpecialEntity, ''Identity, ''LifeTime, ''ActiveScript, ''WorldFloor, ''MovingSpeed, ''Tags, ''ActorState, ''Inventory, ''EditorInstancedFromWorld, ''ActionSet]
   ++ [''Local, ''Player]
   ++ [''AllTiles', ''Chunks, ''WorldTime, ''PaletteManager, ''Random, ''WorldStases, ''FloorProperties, ''Variables, ''IncludeEditorInfo]
   ++ [''Settings, ''UIConfig, ''WorldSpace, ''Dictionary, ''Language, ''ScriptBank, ''RunningScriptEntity, ''ScriptEventBuffer, ''WireReceivers]
   ++ [''ToolMode, ''ToolModeUniversalState]

type System' a = System KathuWorld a
type SystemT' m a = SystemT KathuWorld m a
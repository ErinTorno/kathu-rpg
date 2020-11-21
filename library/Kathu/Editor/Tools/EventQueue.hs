module Kathu.Editor.Tools.EventQueue where

import           Apecs
import           Control.Concurrent.MVar

import           Kathu.Editor.Tools.ToolMode
import           Kathu.Entity.System
import           Kathu.World.Tile            (Tile)
import           Kathu.World.WorldSpace      (InstancedPrefab, WorldSpace)

-- Events that the app receives
data AppEvent
    = TryToQuitGame
    | UseToolMode ToolMode
    | SetSelectedTile Tile
    | LoadWorldSpace  WorldSpace
    | ToggleDebug
    | DestroyEntity Entity
    | PlaceEntity InstancedPrefab
    | FinishEditingEntityInstance

-- Events that the editor receives
data EditorEvent
    = EditEntityInstance Entity InstancedPrefab

data EventQueue = EventQueue
    { entityWorld  :: !(MVar KathuWorld)
    , appEvents    :: !(MVar [AppEvent])
    , editorEvents :: !(MVar [EditorEvent])
    }

-- EventQueue manipulation

newEventQueue :: IO EventQueue
newEventQueue = do
    world   <- newEmptyMVar
    appEvs  <- newMVar []
    toolEvs <- newMVar []
    pure $ EventQueue world appEvs toolEvs
    
pushAppEvent :: EventQueue -> AppEvent -> IO ()
pushAppEvent EventQueue{appEvents = appEvs} event =
    modifyMVar_ appEvs $ pure . (event:)

pushEditorEvent :: EventQueue -> EditorEvent -> IO ()
pushEditorEvent EventQueue{editorEvents = toolEvs} event =
    modifyMVar_ toolEvs $ pure . (event:)

pollAppEvents :: EventQueue -> IO [AppEvent]
pollAppEvents = pollEvents . appEvents

pollEditorEvents :: EventQueue -> IO [EditorEvent]
pollEditorEvents = pollEvents . editorEvents

pollEvents :: MVar [a] -> IO [a]
pollEvents eventMVar = do
    maybeEvents <- tryTakeMVar eventMVar
    case maybeEvents of
        Nothing     -> pure []
        Just events -> do
            putMVar eventMVar []
            pure events

-- EntityWorld

takeEntityWorld :: EventQueue -> IO KathuWorld
takeEntityWorld EventQueue{entityWorld = worldMVar} = takeMVar worldMVar

putEntityWorld :: KathuWorld -> EventQueue -> IO ()
putEntityWorld world EventQueue{entityWorld = worldMVar} = putMVar worldMVar world

runWithEntityWorld :: EventQueue -> SystemT' IO a -> IO a
runWithEntityWorld EventQueue{entityWorld = worldMVar} action =
    withMVar worldMVar $ \world ->
        runWith world action
module Kathu.App.Tools.EventQueue where

import           Apecs
import           Control.Concurrent.MVar

import           Kathu.App.Graphics.Image
import           Kathu.App.System
import           Kathu.App.Tools.ToolMode
import           Kathu.World.Tile            (Tile)
import           Kathu.World.WorldSpace      (InstancedPrototype, WorldSpace)

-- Events that the app receives
data AppEvent
    = TryToQuitGame
    | UseToolMode ToolMode
    | SetSelectedTile (Tile ImageID)
    | LoadWorldSpace  (WorldSpace ImageID)
    | ToggleDebug
    | DestroyEntity Entity
    | PlaceEntity (InstancedPrototype ImageID)
    | FinishEditingEntityInstance

-- Events that the editor receives
data EditorEvent
    = EditEntityInstance Entity (InstancedPrototype ImageID)

data EventQueue = EventQueue
    { entityWorld  :: !(MVar EntityWorld)
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

takeEntityWorld :: EventQueue -> IO EntityWorld
takeEntityWorld EventQueue{entityWorld = worldMVar} = takeMVar worldMVar

putEntityWorld :: EntityWorld -> EventQueue -> IO ()
putEntityWorld world EventQueue{entityWorld = worldMVar} = putMVar worldMVar world

runWithEntityWorld :: EventQueue -> SystemT' IO a -> IO a
runWithEntityWorld EventQueue{entityWorld = worldMVar} action =
    withMVar worldMVar $ \world ->
        runWith world action
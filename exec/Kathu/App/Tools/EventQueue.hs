module Kathu.App.Tools.EventQueue where

import           Apecs
import           Control.Concurrent.MVar
import           Control.Lens
import qualified Data.Map                    as Map

import           Kathu.App.Data.Library
import           Kathu.App.Graphics.Image
import           Kathu.App.System
import           Kathu.App.Tools.Commands
import           Kathu.App.Tools.ToolMode
import           Kathu.App.Tools.ToolSystem
import           Kathu.App.World             (loadWorldSpace)
import           Kathu.Entity.System         (Debug(..))
import           Kathu.World.Tile            (Tile)
import           Kathu.World.WorldSpace      (WorldSpace, worldID)

-- Events that the app receives
data AppEvent
    = UseToolMode ToolMode
    | SetSelectedTile (Tile ImageID)
    | LoadWorldSpace  (WorldSpace ImageID)
    | ToggleDebug
    | RunSystem (SystemT' IO ())

-- Events that the editor receives
data EditorEvent
    = TryToCloseEditor

data EventQueue = EventQueue
    { entityWorld  :: !(MVar EntityWorld)
    , appEvents    :: !(MVar [AppEvent])
    , editorEvents :: !(MVar [EditorEvent])
    }

-- Polls for all events in the EventQueue, and returns True if the normal game runner should run after this
handleEvents :: EventQueue -> CommandState -> SystemT' IO ()
handleEvents queue  _ = do
    events <- lift $ pollAppEvents queue
    mapM_ handleEvent events

handleEvent :: AppEvent -> SystemT' IO ()
handleEvent event = case event of
    RunSystem action ->
        action
    ToggleDebug -> do
        Debug isDebug <- get global
        global        $= Debug (not isDebug)
    UseToolMode newMode ->
        handleUseToolModeEvent newMode
    SetSelectedTile sTile -> do
        toolUnivSt <- get global
        global $= toolUnivSt {selectedTile = sTile}
    LoadWorldSpace worldspace -> do
        -- Update it in the library
        library <- get global
        global  $= over worldSpaces (Map.insert (worldspace^.worldID) worldspace) library
        loadWorldSpace worldspace

-- EventQueue manipulation

newEventQueue :: IO EventQueue
newEventQueue = do
    world   <- newEmptyMVar
    appEvs  <- newMVar []
    toolEvs <- newMVar []
    pure $ EventQueue world appEvs toolEvs
    
pushAppEvent :: EventQueue -> AppEvent -> IO ()
pushAppEvent EventQueue{appEvents = appEvs} event = do
    events <- takeMVar appEvs
    putMVar appEvs $ events |> event

pushEditorEvent :: EventQueue -> EditorEvent -> IO ()
pushEditorEvent EventQueue{editorEvents = toolEvs} event = do
    events <- takeMVar toolEvs
    putMVar toolEvs $ events |> event

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
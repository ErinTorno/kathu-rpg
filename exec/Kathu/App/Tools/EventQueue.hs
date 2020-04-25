{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.App.Tools.EventQueue where

import           Apecs
import           Apecs.Physics
import           Control.Concurrent.MVar
import           Control.Monad               (forM_, mapM_, when)
import           Control.Lens
import qualified Data.Map                    as Map
import           Linear.V2                   (V2(..))

import           Kathu.App.Data.Library
import           Kathu.App.Graphics.Image
import           Kathu.App.System
import           Kathu.App.Tools.Commands
import           Kathu.App.Tools.ToolMode
import           Kathu.App.World             (loadWorldSpace)
import           Kathu.Entity.Components     (Local, simpleIdentity)
import           Kathu.Graphics.Camera
import           Kathu.Util.Apecs
import           Kathu.World.WorldSpace      (WorldSpace, emptyWorldSpace, worldID)
    
-- Events that the app receives
data AppEvent
    = UseToolMode ToolMode
    | LoadWorldSpace (WorldSpace ImageID)
    | NewWorldSpace
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
    RunSystem action -> action
    UseToolMode newMode -> do
        prevMode <- get global
        global   $= newMode
        -- the new mode is a free cam mode, but not the last
        -- let's create a new free cam entity, and give it the camera
        when (usesFreeCam newMode && not (usesFreeCam prevMode)) $ do
            playerEty  <- getUnique
            cmap $ \(_ :: Camera) -> Not :: Not Camera

            freeCamEty <- newEntity (simpleIdentity "ToolMode Free Camera", DynamicBody)
            case playerEty of
                Just (cam :: Camera, p :: Position) -> freeCamEty $= (cam, p)
                Nothing                             -> freeCamEty $= (defaultCamera, Position (V2 0 0))
        -- the new mode is not a free cam mode, but the last is
        -- let's destroy the free cam entity, and give the camera back to the player
        when (usesFreeCam prevMode && not (usesFreeCam newMode)) $ do
            lastCamEty <- getUnique
            playerEty  <- getUnique
            -- Removes camera, then destroys free cam entity
            forM_ lastCamEty $ \(_ :: Camera, ety) -> do
                cmap $ \(_ :: Camera) -> Not :: Not Camera
                destroyEntity ety
            -- Gives the camera to the player
            forM_ playerEty $ \(_ :: Local, ety) ->
                ety $= defaultCamera
    NewWorldSpace ->
        loadWorldSpace emptyWorldSpace
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
takeEntityWorld EventQueue {entityWorld = worldMVar} = takeMVar worldMVar

putEntityWorld :: EntityWorld -> EventQueue -> IO ()
putEntityWorld world EventQueue {entityWorld = worldMVar} = putMVar worldMVar world

runWithEntityWorld :: EventQueue -> SystemT' IO a -> IO a
runWithEntityWorld EventQueue {entityWorld = worldMVar} action = do
    world <- takeMVar worldMVar
    val   <- runWith world action
    putMVar worldMVar world
    pure val
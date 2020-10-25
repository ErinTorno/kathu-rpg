module Kathu.App.Tools.EventHandler where

import           Apecs
import           Control.Lens
import           Control.Monad               (when)
import qualified Data.Map                    as Map
import           Verda.World                 (IsDebug(..), IsQuitting(..))

import           Kathu.App.Data.Dictionary
import           Kathu.App.System
import           Kathu.App.Tools.Commands
import           Kathu.App.Tools.EventQueue
import           Kathu.App.Tools.ToolMode
import           Kathu.App.Tools.ToolSystem
import           Kathu.App.World             (loadWorldSpace)
import           Kathu.World.WorldSpace

-- Polls for all events in the EventQueue, and returns True if the normal game runner should run after this
handleEvents :: EventQueue -> CommandState -> SystemT' IO ()
handleEvents queue  _ = do
    events <- lift $ pollAppEvents queue
    -- reverse so oldest events are processed first
    mapM_ handleEvent . reverse $ events

handleEvent :: AppEvent -> SystemT' IO ()
handleEvent event = case event of
    TryToQuitGame ->
        global $= IsQuitting True
    ToggleDebug -> do
        IsDebug isDebug <- get global
        global        $= IsDebug (not isDebug)
    UseToolMode newMode ->
        handleUseToolModeEvent newMode
    SetSelectedTile sTile -> do
        toolUnivSt <- get global
        global $= toolUnivSt {selectedTile = sTile}
    LoadWorldSpace worldspace -> do
        -- Update it in the dictionary
        dictionary <- get global
        global  $= over dictWorldSpaces (Map.insert (worldspace^.worldID) worldspace) dictionary
        loadWorldSpace worldspace
    DestroyEntity ety ->
        destroyEntity ety
    PlaceEntity instancedPrefab -> do
        includeEditorInfo <- get global
        placeInstancedPrefab includeEditorInfo newFromPrefabWithScriptMapping instancedPrefab
        -- If we can edit entities, then we must rebuild the editor info collisions to include this new entity
        toolmode <- get global
        when (canEditEntities toolmode)
            rebuildEntityInfoCollisions
    FinishEditingEntityInstance ->
        modify global $ \univToolSt -> univToolSt {canEditEntityInstance = True}
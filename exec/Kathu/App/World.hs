module Kathu.App.World where

import           Apecs
import           Control.Lens
import           Control.Monad                   (void)
import           Verda.Graphics.SpriteManager

import           Kathu.App.System
import qualified Kathu.Scripting.Lua             as Lua
import           Kathu.World.WorldSpace

loadWorldSpace :: WorldSpace -> SystemT' IO ()
loadWorldSpace ws = do
    prevManager <- get global
    manager <- loadPalettes (ws^.worldPalettes) prevManager
    global  $= manager
    initWorldSpace destroyEntity newFromPrefabWithScriptMapping (Lua.loadScript id externalFunctions) ws
    void $ setPaletteManager (ws^.initialPalette)

    -- to prevent pausing issues during gameplay, we force a GC now while it's just done loading
    runGC

rebuildCurrentTileCollisions :: SystemT' IO ()
rebuildCurrentTileCollisions = do
    worldspace :: WorldSpace <- get global
    destroyTileCollisions destroyEntity
    buildTileCollisions worldspace
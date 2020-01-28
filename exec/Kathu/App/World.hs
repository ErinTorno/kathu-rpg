module Kathu.App.World where

import           Apecs
import           Control.Monad                   (void)

import           Kathu.App.Graphics.Image        (ImageID)
import           Kathu.App.Graphics.ImageManager
import           Kathu.App.System                (SystemT', destroyEntity, externalFunctions, newFromPrototype)
import qualified Kathu.Scripting.Lua             as Lua
import           Kathu.World.WorldSpace

loadWorldSpace :: WorldSpace ImageID -> SystemT' IO ()
loadWorldSpace ws = do
    prevManager <- get global
    manager <- loadPalettes (worldPalettes ws) prevManager
    global  $= manager
    initWorldSpace destroyEntity newFromPrototype (Lua.loadScript externalFunctions) ws
    void $ setPaletteManager (initialPalette ws)

    -- to prevent pausing issues during gameplay, we force a GC now while it's just done loading
    runGC
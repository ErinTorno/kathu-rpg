module Kathu.App.World where

import Apecs

import Kathu.App.Graphics.Image (ImageID)
import Kathu.App.Graphics.ImageManager
import Kathu.App.System (SystemT')
import Kathu.Entity.Prototype
import Kathu.World.WorldSpace

loadWorldSpace :: WorldSpace ImageID -> SystemT' IO ()
loadWorldSpace ws = do
    prevManager <- get global
    manager <- loadPalettes (worldPalettes ws) prevManager
    global  $= manager
    initWorldSpace newFromPrototype ws
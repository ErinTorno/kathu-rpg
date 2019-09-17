{-# LANGUAGE OverloadedStrings #-}

module Kathu.App.Init (entityWorld, localPlayer, system) where

import Apecs hiding (get)
import Control.Lens
import qualified Data.Map as Map
import Data.Maybe (maybe)
import qualified System.Random as R

import Kathu.App.Data.Library
import Kathu.App.Data.Settings
import Kathu.App.Graphics.ImageManager
import Kathu.App.System
import Kathu.App.World (loadWorldSpace)
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.Prototype
import Kathu.Entity.System
import Kathu.IO.Directory (assetPath)
import Kathu.Graphics.Camera

entityWorld :: IO EntityWorld
entityWorld = initEntityWorld

-- initializes an entity as the local player
localPlayer :: Entity -> SystemT' IO ()
localPlayer ety = do
    ety $= Camera 1.0
    ety $= Local emptyActionPressed
    ety $= emptyActionSet

system :: Settings -> SystemT' IO ()
system settings = do
    library <- lift (loadLibrary mempty assetPath)
    seed    <- lift . maybe (R.randomIO :: IO Int) pure . randomSeed $ settings
    manager <- lift $ mkImageManager (view images library)
    global $= library
    global $= manager
    global $= Random (R.mkStdGen seed)
    global $= settings
    global $= library ^. uiConfig
    let getLib g t = (view g library) Map.! t

    playerEty <- newFromPrototype $ getLib prototypes "player"
    localPlayer playerEty

    let worldspace = getLib worldSpaces . initialWorld $ settings
    loadWorldSpace worldspace
    
    pure ()
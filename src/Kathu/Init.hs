{-# LANGUAGE OverloadedStrings #-}

module Kathu.Init (entityWorld, localPlayer, system) where

import Apecs hiding (get)
import Control.Lens
import qualified Data.Map as Map
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.Prototype
import Kathu.Entity.System
import Kathu.IO.File (assetPath)
import Kathu.IO.Library
import Kathu.IO.Settings
import Kathu.Graphics.Camera
import Kathu.Graphics.Color
import Kathu.Graphics.Drawable
import Kathu.Graphics.UI
import Kathu.Graphics.ImageManager
import Kathu.World.WorldSpace
import Kathu.World.WorldSystem
import qualified SDL
import qualified System.Random as R


entityWorld :: IO EntityWorld
entityWorld = initEntityWorld

-- initializes an entity as the local player
localPlayer :: Entity -> SystemT' IO ()
localPlayer ety = do
    ety $= Camera 1.0
    ety $= Local emptyActionPressed
    ety $= emptyActionSet

system :: SDL.Renderer -> Settings -> SystemT' IO ()
system renderer settings = do
    library <- lift (loadLibrary renderer assetPath)
    seed    <- lift (R.randomIO :: IO Int)
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
    initWorldSpace worldspace

    -- testing set for now; will change in future to be else where
    
    pure ()
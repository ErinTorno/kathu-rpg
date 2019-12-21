{-# LANGUAGE OverloadedStrings #-}

module Kathu.App.Init (entityWorld, localPlayer, system) where

import Apecs hiding (get)
import Apecs.Physics
import Control.Lens hiding (Identity)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import Linear.V2 (V2(..))
import qualified SDL as SDL
import qualified System.Random as R

import Kathu.App.Data.Library
import Kathu.App.Data.Settings
import Kathu.App.Graphics.Font (initFontCache)
import Kathu.App.Graphics.ImageManager
import Kathu.App.Graphics.UI
import Kathu.App.System
import Kathu.App.World (loadWorldSpace)
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.Physics.Floor
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

system :: SDL.Window -> SDL.Renderer -> Settings -> SystemT' IO ()
system window renderer settings = do
    (library, surfaces) <- lift . loadLibrary mempty $ assetPath
    seed    <- lift . maybe (R.randomIO :: IO Int) pure . randomSeed $ settings
    manager <- lift . mkImageManager renderer $ surfaces
    tilesV  <- lift . makeTiles . view tiles $ library
    global  $= library
    global  $= manager
    global  $= Random (R.mkStdGen seed)
    global  $= tilesV
    global  $= settings
    global  $= library^.uiConfig
    global  $= (Gravity $ V2 0 0) -- no gravity, as the game is top-down

    setWindowIcon window (library^.uiConfig.to gameIcon)

    floorPropEtys <- mapM initFloorProperty . view floorProperties $ library
    global  $= FloorProperties (floorPropEtys Map.! "default") floorPropEtys

    fontCache <- initFontCache renderer (Map.singleton "" (library^.font))
    global    $= fontCache

    let getLib g t = (view g library) Map.! t
    
    playerEty <- newFromPrototype $ getLib prototypes "player"
    localPlayer playerEty

    let worldspace = getLib worldSpaces . initialWorld $ settings
    loadWorldSpace worldspace
    
    pure ()
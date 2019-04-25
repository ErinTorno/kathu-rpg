{-# LANGUAGE OverloadedStrings #-}

module Kathu.Init (entityWorld, localPlayer, system) where

import Apecs hiding (get)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.IO.File (assetPath)
import Kathu.IO.Library
import Kathu.IO.Settings
import qualified SDL
import System.Exit (exitFailure)
import System.IO
import qualified System.Random as R

entityWorld = initEntityWorld

-- initializes an entity as the local player
localPlayer :: Settings -> Entity -> SystemT' IO ()
localPlayer settings ety = do
    ety $= Camera (resolutionX settings) (resolutionY settings) 1.0
    ety $= Local emptyActionPressed
    ety $= emptyActionSet

system :: SDL.Renderer -> Settings -> SystemT' IO ()
system renderer settings = do
    library <- lift (loadLibrary renderer assetPath)
    seed    <- lift (R.randomIO :: IO Int)
    global $= library
    global $= Random (R.mkStdGen seed)
    global $= settings

    -- testing set for now; will change in future to be else where

    playerEty <- newFromPrototype $ (prototypes library) Map.! "player"
    localPlayer settings playerEty
    
    boulderEty <- newFromPrototype $ (prototypes library) Map.! "boulder"

    pure ()
{-# LANGUAGE OverloadedStrings #-}

module Init where

import Apecs
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Random as R

import Entity.Action
import Entity.Components
import Entity.System
import IO.File (assetPath)
import IO.Library
import IO.Settings

entityWorld = initEntityWorld

-- initializes an entity as the local player
localPlayer :: Settings -> Entity -> SystemT' IO ()
localPlayer settings ety = do
    set ety $ Camera (resolutionX settings) (resolutionY settings) 1.0
    set ety $ Local emptyActionPressed
    set ety $ emptyActionSet

system :: Settings -> SystemT' IO ()
system settings = do
    library <- lift (loadLibrary assetPath)
    seed    <- lift (R.randomIO :: IO Int)
    set global $ library
    set global $ Random (R.mkStdGen seed)
    set global $ settings

    -- testing set for now; will change in future to be else where

    playerEty <- newFromPrototype $ (prototypes library) Map.! "player"
    localPlayer settings playerEty
    
    boulderEty <- newFromPrototype $ (prototypes library) Map.! "boulder"

    pure ()
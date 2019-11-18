{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We need orphaned instances of SDL ScanCodes and KeyCodes to serialize settings

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.App.Data.Settings where

import Data.Aeson
import Data.Bool
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
import Data.Word
import GHC.Generics
import Linear.V2
import qualified SDL

import Kathu.IO.File
import Kathu.Parsing.Aeson
import Kathu.Util.Types (Identifier)

data Settings = Settings
    { targetFPS     :: Float
    , resolution    :: V2 Word32
    , serverPort    :: Word32
    --, language :: 
    , modDir        :: Text
    , baseSaveDir   :: Text
    , moddedSaveDir :: Text
    , canUseDebug   :: Bool
    , controls      :: Controls
    , randomSeed    :: Maybe Int -- if found, will override the random seed generated on game start
    , misc          :: Map Text Text
    -- this is for testing purposes, and will be removed later
    , initialWorld  :: Identifier
    } deriving (Generic)

instance ToJSON Settings where
    toJSON = genericToJSON standardProjectOptions
instance FromJSON Settings where
    parseJSON = genericParseJSON standardProjectOptions

defaultSettings :: Settings
defaultSettings = Settings
    { targetFPS     = 120.0
    , resolution    = V2 1280 720
    , serverPort    = 7777
    , modDir        = "/mods"
    , baseSaveDir   = "/saves"
    , moddedSaveDir = "/modded-saves"
    , canUseDebug   = False
    , controls      = defaultControls
    , randomSeed    = Nothing
    , misc          = Map.empty
    , initialWorld  = "test-world"
    }

data Controls = Controls
    { keyMoveNorth :: SDL.Scancode
    , keyMoveEast  :: SDL.Scancode
    , keyMoveSouth :: SDL.Scancode
    , keyMoveWest  :: SDL.Scancode
    , keyToggleDebug  :: SDL.Scancode
    , keyDebugZoomIn  :: SDL.Scancode
    , keyDebugZoomOut :: SDL.Scancode
    , keyDebugNextPalette :: SDL.Scancode
    , keyDebugPrintPhysics :: SDL.Scancode
    } deriving (Generic)

-- We also need to store keys and scancodes to make use of Control serialization

instance ToJSON SDL.Scancode where
    toJSON (SDL.Scancode un) = toJSON un
instance FromJSON SDL.Scancode where
    parseJSON o = SDL.Scancode <$> parseJSON o

instance ToJSON SDL.Keycode
instance FromJSON SDL.Keycode

instance ToJSON Controls where
    toJSON = genericToJSON standardProjectOptions
instance FromJSON Controls where
    parseJSON = genericParseJSON standardProjectOptions

defaultControls :: Controls
defaultControls = Controls
    { keyMoveNorth = SDL.ScancodeW
    , keyMoveEast  = SDL.ScancodeD
    , keyMoveSouth = SDL.ScancodeS
    , keyMoveWest  = SDL.ScancodeA
    , keyToggleDebug  = SDL.ScancodeF3
    , keyDebugZoomIn  = SDL.ScancodeKPMinus
    , keyDebugZoomOut = SDL.ScancodeKPPlus
    , keyDebugNextPalette = SDL.ScancodeF5
    , keyDebugPrintPhysics = SDL.ScancodeF7
    }

loadSettings :: IO Settings
loadSettings = fileExists fileName >>= bool (pure Nothing) (maybeLoad fileName) >>= def
    where fileName     = "./settings.config"
          def Nothing  = saveToFile FormatYAML fileName defaultSettings >> pure defaultSettings
          def (Just s) = pure s

resolutionX, resolutionY :: Settings -> Word32
resolutionX settings = let (V2 x _) = resolution settings in x
resolutionY settings = let (V2 _ y) = resolution settings in y
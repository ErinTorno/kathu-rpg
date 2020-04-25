{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We need orphaned instances of SDL ScanCodes and KeyCodes to serialize settings

{-# LANGUAGE DeriveGeneric     #-}
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
    { targetFPS      :: Float
    , resolution     :: V2 Word32
    , isVSyncEnabled :: Bool
    , serverPort     :: Word32
    , language       :: Identifier
    , modDir         :: Text
    , baseSaveDir    :: Text
    , moddedSaveDir  :: Text
    , canUseDebug    :: Bool
    , controls       :: Controls
    , randomSeed     :: Maybe Int -- if found, will override the random seed generated on game start
    , misc           :: Map Text Text
    -- this is for testing purposes, and will be removed later
    , initialWorld   :: Maybe Identifier
    } deriving (Generic)

instance ToJSON Settings where
    toJSON = genericToJSON standardProjectOptions
instance FromJSON Settings where
    parseJSON = genericParseJSON standardProjectOptions

defaultSettings :: Settings
defaultSettings = Settings
    { targetFPS      = 60.0
    , resolution     = V2 1280 720
    , isVSyncEnabled = False
    , serverPort     = 7777
    , language       = "english"
    , modDir         = "/mods"
    , baseSaveDir    = "/saves"
    , moddedSaveDir  = "/modded-saves"
    , canUseDebug    = True
    , controls       = defaultControls
    , randomSeed     = Nothing
    , misc           = Map.empty
    , initialWorld   = Just "test-world"
    }

data Controls = Controls
    { keyMoveNorth         :: SDL.Scancode
    , keyMoveEast          :: SDL.Scancode
    , keyMoveSouth         :: SDL.Scancode
    , keyMoveWest          :: SDL.Scancode
    , keyFocus             :: SDL.Scancode
    , keyToggleDebug       :: SDL.Scancode
    , keyDebugZoomIn       :: SDL.Scancode
    , keyDebugZoomOut      :: SDL.Scancode
    , keyDebugNextPalette  :: SDL.Scancode
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
    { keyMoveNorth         = SDL.ScancodeW
    , keyMoveEast          = SDL.ScancodeD
    , keyMoveSouth         = SDL.ScancodeS
    , keyMoveWest          = SDL.ScancodeA
    , keyFocus             = SDL.ScancodeLShift
    , keyToggleDebug       = SDL.ScancodeF3
    , keyDebugZoomIn       = SDL.ScancodeKPMinus
    , keyDebugZoomOut      = SDL.ScancodeKPPlus
    , keyDebugNextPalette  = SDL.ScancodeF5
    , keyDebugPrintPhysics = SDL.ScancodeF7
    }

settingsFileName :: String
settingsFileName = "./settings.yaml"

saveSettings :: Settings -> IO ()
saveSettings = saveToFile FormatYAML settingsFileName

loadSettings :: IO Settings
loadSettings = fileExists settingsFileName >>= bool (pure Nothing) (maybeLoad settingsFileName) >>= def
    where def Nothing  = saveSettings defaultSettings >> pure defaultSettings
          def (Just s) = pure s
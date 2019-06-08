{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.IO.Settings where

import Data.Aeson
import Data.Bool
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text
import Data.Word
import GHC.Generics
import Kathu.IO.File
import Kathu.IO.Misc
import Kathu.IO.Parsing (projectOptions)
import Linear.V2
import qualified SDL

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
    , misc          :: Map Text Text
    -- this is for testing purposes, and will be removed later
    , initialWorld  :: Text
    } deriving (Generic)

instance ToJSON Settings where
    toJSON = genericToJSON projectOptions
instance FromJSON Settings where
    parseJSON = genericParseJSON projectOptions

defaultSettings = Settings
    { targetFPS     = 120.0
    , resolution    = V2 1280 720
    , serverPort    = 7777
    , modDir        = "/mods"
    , baseSaveDir   = "/saves"
    , moddedSaveDir = "/modded-saves"
    , canUseDebug   = False
    , controls      = defaultControls
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
    } deriving (Generic)

instance ToJSON Controls where
    toJSON = genericToJSON projectOptions
instance FromJSON Controls where
    parseJSON = genericParseJSON projectOptions

defaultControls = Controls
    { keyMoveNorth = SDL.ScancodeW
    , keyMoveEast  = SDL.ScancodeD
    , keyMoveSouth = SDL.ScancodeS
    , keyMoveWest  = SDL.ScancodeA
    , keyToggleDebug  = SDL.ScancodeF3
    , keyDebugZoomIn  = SDL.ScancodeKPMinus
    , keyDebugZoomOut = SDL.ScancodeKPPlus
    , keyDebugNextPalette = SDL.ScancodeF5
    }

loadSettings :: IO Settings
loadSettings = fileExists fd >>= bool (pure Nothing) (maybeLoad fd) >>= def
    where fd = "./settings.config"
          def Nothing  = saveToFile FormatYAML fd defaultSettings >> pure defaultSettings
          def (Just s) = pure s

resolutionX settings = let (V2 x _) = resolution settings in x
resolutionY settings = let (V2 _ y) = resolution settings in y
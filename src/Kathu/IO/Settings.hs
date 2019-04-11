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
import Linear.V2

data Settings = Settings
    { targetFPS :: Float
    , resolution :: V2 Word32
    , serverPort :: Word32
    --, language :: 
    , modDir :: Text
    , baseSaveDir :: Text
    , moddedSaveDir :: Text
    , misc :: Map Text Text
    } deriving (Generic)

instance ToJSON Settings
instance FromJSON Settings

defaultSettings = Settings
    { targetFPS     = 120.0
    , resolution    = V2 1280 720
    , serverPort    = 7777
    , modDir        = "/mods"
    , baseSaveDir   = "/saves"
    , moddedSaveDir = "/modded-saves"
    , misc = Map.empty
    }

loadSettings :: IO Settings
loadSettings = fileExists fd >>= bool (pure Nothing) (maybeLoad fd) >>= def
    where fd = "./settings.config"
          def Nothing  = saveToFile FormatYAML fd defaultSettings >> pure defaultSettings
          def (Just s) = pure s

resolutionX settings = let (V2 x _) = resolution settings in x
resolutionY settings = let (V2 _ y) = resolution settings in y
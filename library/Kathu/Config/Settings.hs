module Kathu.Config.Settings where

import           Apecs                   hiding (Map)
import           Data.Aeson
import           Data.Bool
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text
import           Data.Word
import           GHC.Generics
import           Linear.V2
import           Verda.IO.Files
import           Verda.Parsing.Aeson
import           Verda.Util.Types        (Identifier)

import           Kathu.Config.Controls

data Settings = Settings
    { targetFPS      :: !Double
    , resolution     :: !(V2 Word32)
    , isVSyncEnabled :: !Bool
    , language       :: !Identifier
    , modDir         :: !Text
    , baseSaveDir    :: !Text
    , moddedSaveDir  :: !Text
    , canUseDebug    :: !Bool
    , controls       :: !Controls
    , randomSeed     :: !(Maybe Int) -- if found, will override the random seed generated on game start
    , misc           :: !(Map Text Text)
    -- this is for testing purposes, and will be removed later
    , initialWorld   :: !(Maybe Identifier)
    } deriving (Generic)

instance ToJSON Settings where
    toJSON = genericToJSON standardProjectOptions
instance FromJSON Settings where
    parseJSON = genericParseJSON standardProjectOptions

instance Semigroup Settings where (<>) = mappend
instance Monoid Settings where mempty = defaultSettings
instance Component Settings where type Storage Settings = Global Settings

defaultSettings :: Settings
defaultSettings = Settings
    { targetFPS      = 60
    , resolution     = V2 1280 720
    , isVSyncEnabled = False
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

settingsFileName :: String
settingsFileName = "./settings.yaml"

saveSettings :: Settings -> IO ()
saveSettings = saveToFile FormatYAML settingsFileName

loadSettings :: IO Settings
loadSettings = fileExists settingsFileName >>= bool (pure Nothing) (maybeLoad settingsFileName) >>= def
    where def Nothing  = saveSettings defaultSettings >> pure defaultSettings
          def (Just s) = pure s
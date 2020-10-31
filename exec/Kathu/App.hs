module Kathu.App (Kathu.App.start) where

import           Apecs
import           Apecs.Physics                   (stepPhysics)
import           Control.Monad                   (when)
import           Verda.App
import           Verda.Graphics.Icons            (loadIcon)
import           Verda.World

import           Kathu.App.Events
import qualified Kathu.App.Init                  as Init
import           Kathu.App.System
import           Kathu.Editor.Main               as Editor
import qualified Kathu.Config.Settings           as Settings
import qualified Kathu.Game                      as Game

mkAppConfig :: IO (AppConfig EntityWorld)
mkAppConfig = do
    settings <- Settings.loadSettings
    pure $ AppConfig
        { appName            = "Kathu"
        , resolution         = fromIntegral <$> Settings.resolution settings
        , updateHertz        = 60 -- make this and fps unbounded optionally
        , renderHertz        = Settings.targetFPS settings
        , appIcon            = loadIcon "assets/icon-large.png"
        , appWorld           = Init.entityWorld
        , initWorld          = Init.system settings
        , concurrentWorldVar = pure Nothing
        , runGame            = \updateDelay -> do
            runEvents
            Game.runGame destroyEntity updateDelay
            runState <- get global
            when (runState == Running) $
                stepPhysics (fromIntegral updateDelay / 1000)
        }

start :: [String] -> IO ()
start args
    | Editor.shouldRunEditor args = mkAppConfig >>= Editor.start args
    | otherwise                   = mkAppConfig >>= run
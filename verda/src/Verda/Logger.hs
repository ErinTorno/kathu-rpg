module Verda.Logger where

import           Apecs
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import qualified Data.Text.IO           as T

data LogType = Info | Warning | Error deriving Show

newtype Logger = Logger {unLogger :: LogType -> Text -> IO ()}

instance Semigroup Logger where (<>) = mappend
instance Monoid Logger where mempty  = defaultLogger
instance Component Logger where type Storage Logger = Global Logger

logTypeText :: LogType -> Text
logTypeText Info    = "Info"
logTypeText Warning = "Warning"
logTypeText Error   = "Error"

defaultLogger :: Logger
defaultLogger = Logger $ \typ msg -> T.putStr (logTypeText typ)
                                  >> T.putStr ": "
                                  >> T.putStrLn msg

logLine :: forall w m. (MonadIO m, Get w m Logger) => LogType -> Text -> SystemT w m ()
logLine typ msg = do
    Logger logFn <- get global
    liftIO $ logFn typ msg
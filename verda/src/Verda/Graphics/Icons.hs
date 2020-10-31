module Verda.Graphics.Icons where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.Text              as T
import qualified SDL
import qualified SDL.Image              as SDLI
import qualified SDL.Internal.Types     as SDLInternal
import qualified SDL.Raw.Video          as SDLVRaw

import           Verda.IO.Directory     (WorkingDirectory, resolveAssetPathDP)
import           Verda.Util.Dependency

newtype Icon = Icon {unIcon :: SDL.Surface}

instance (s `CanProvide` WorkingDirectory, MonadIO m) => FromJSON (Dependency s m Icon) where
    parseJSON (String s) = pure $ (resolveAssetPathDP . T.unpack) s
                       >>= liftDependency . loadIcon
    parseJSON v          = typeMismatch "Icon" v

loadIcon :: MonadIO m => FilePath -> m Icon
loadIcon = fmap Icon . SDLI.load

setWindowIcon :: MonadIO m => SDL.Window -> Icon -> m ()
setWindowIcon (SDLInternal.Window window) (Icon (SDL.Surface surPtr _)) = SDLVRaw.setWindowIcon window surPtr
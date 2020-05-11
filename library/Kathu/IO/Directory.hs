module Kathu.IO.Directory where

import Data.Aeson
import GHC.Generics
import Data.String           (IsString)

import Kathu.Util.Dependency

-- | A wrapper around a file path to use from parsing files relative to a directory
newtype WorkingDirectory = WorkingDirectory {unWorkingDir :: FilePath}
    deriving (Show, Eq, Ord, Generic, IsString, ToJSON, FromJSON)

assetPath :: IsString a => a
assetPath = "assets"

-- | Strips unnecessary-but-informative `./` from relative file paths
sanitizeRelativePath :: FilePath -> FilePath
sanitizeRelativePath ('.':'/':ending) = ending
sanitizeRelativePath ending           = ending

-- Strings that begin with / are absolute, otherwise they are relative to the current directory
-- as such, using C:\ or similar notations to access outside of the directory is not allowed
-- Current Directory -> Given Path -> Absolute Path
resolveAssetPath :: FilePath -> FilePath -> FilePath
resolveAssetPath _ ('/':cs) = concat [assetPath, "/", cs]
resolveAssetPath "" ending  = concat [assetPath, "/", sanitizeRelativePath ending]
resolveAssetPath p ending   = concat [p, "/", sanitizeRelativePath ending]

resolveAssetPathDP :: (s `CanProvide` WorkingDirectory, Monad m) => FilePath -> Dependency s m FilePath
resolveAssetPathDP path = flip resolveAssetPath path . unWorkingDir <$> provide
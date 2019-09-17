{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds, TypeOperators #-}

module Kathu.IO.Directory where

import Data.Aeson
import GHC.Generics
import Data.List (isPrefixOf)
import Data.String (IsString)
import System.FilePath (FilePath)

import Kathu.Util.Dependency

-- | A wrapper around a file path to use from parsing files relative to a directory
newtype WorkingDirectory = WorkingDirectory {unWorkingDir :: FilePath}
    deriving (Show, Eq, Ord, Generic, IsString, ToJSON, FromJSON)

assetPath :: FilePath
assetPath = "./assets"

-- Strings that begin with / are absolute, otherwise they are relative to the current directory
-- as such, using C:\\ or similar notations to access outside of the directory is not allowed
resolveAssetPath :: FilePath -> FilePath -> FilePath
resolveAssetPath _ ('/':cs) = concat [assetPath, "/", cs]
resolveAssetPath "" ending  = concat [assetPath, "/", ending]
resolveAssetPath p ending | assetPath `isPrefixOf` p = concat [p, "/", ending]
                          | otherwise                = concat [assetPath, "/", p, "/", ending]

resolveAssetPathDP :: (s `CanProvide` WorkingDirectory, Monad m) => FilePath -> Dependency s m FilePath
resolveAssetPathDP path = ((flip resolveAssetPath) path . unWorkingDir) <$> provide
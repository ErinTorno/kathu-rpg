{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- Meant to be imported qualified

module Kathu.Scripting.Lua.Types where

import           Apecs                       (Component, Map, Storage, SystemT)
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.Types            (typeMismatch)
import           Data.Functor.Compose
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.Text                   as T
import qualified Foreign.Lua.Core            as Lua

import           Kathu.IO.Directory
import           Kathu.Parsing.Aeson
import           Kathu.Scripting.Event
import           Kathu.Util.Dependency
import           Kathu.Util.Types

data Script = Script
    { mainScript       :: !ByteString -- this is main script
    , scriptEventFlags :: !EventFlag
    }

instance (s `CanProvide` WorkingDirectory, MonadIO m) => FromJSON (Dependency s m Script) where
    parseJSON e = let readF f = liftDependency . liftIO . BS.readFile =<< (resolveAssetPathDP . T.unpack) f
                   in case e of
        (String s) -> pure $ Script <$> readF s <*> pure noEventFlags
        (Object v) -> getCompose $ Script <$> Compose (readF <$> v .: "file") <*> v .:^? "events" .!=~ noEventFlags
        v          -> typeMismatch "Script" v

data ActiveScript = ActiveScript
    { activeState :: !(MVar Lua.State)
    , eventFlags  :: !EventFlag
    }

instance Component ActiveScript where type Storage ActiveScript = Map ActiveScript

-- | A collection of functions that are not currently possible to implement with just the library project, but are required for scripts to run
data ExternalFunctions w = ExternalFunctions
    { setPalette :: Identifier -> SystemT w IO Bool -- Identifier to change to -> True if successful change
    }
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

import           Apecs                       (Component, Map, Storage)
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
    { scriptID         :: !Identifier -- a unique identifier to refer to this script (usually its file path)
    , mainScript       :: !ByteString -- this is main script
    , scriptEventFlags :: !EventFlag
    , isSingleton      :: !Bool       -- if True, then there all users of this script share the same instance and state
    }

instance (s `CanProvide` WorkingDirectory, MonadIO m) => FromJSON (Dependency s m Script) where
    parseJSON e = let readF f = do path <- (resolveAssetPathDP . T.unpack) f
                                   bstr <- liftDependency . liftIO . BS.readFile $ path
                                   pure $ Script (mkIdentifier . T.pack $ path) bstr
                   in case e of
        (String s) -> pure $ readF s <*> pure noEventFlags <*> pure False
        (Object v) -> getCompose $ Compose (readF <$> v .: "file")
                  <*> v .:^? "events" .!=~ noEventFlags
                  <*> v .:^? "is-singleton" .!=~ False
        v          -> typeMismatch "Script" v

data ActiveScript = ActiveScript
    { activeState :: !(MVar Lua.State)
    , eventFlags  :: !EventFlag
    }

instance Component ActiveScript where type Storage ActiveScript = Map ActiveScript

newtype ScriptBank = ScriptBank {unScriptBank :: IDHashTable ActiveScript }
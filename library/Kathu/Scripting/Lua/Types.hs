{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- Meant to be imported qualified

module Kathu.Scripting.Lua.Types where

import           Apecs
import           Control.Concurrent.MVar
import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.Types            (typeMismatch)
import           Data.Functor.Compose
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.Text                   as T
import           Foreign.Lua                 (Lua)
import qualified Foreign.Lua                 as Lua
import qualified Foreign.Lua.FunctionCalling as Lua
import           Data.Vector                 (Vector)

import           Kathu.IO.Directory
import           Kathu.Parsing.Aeson
import           Kathu.Scripting.Event
import           Kathu.Scripting.Variables   (WatchedVariable)
import           Kathu.Util.Apecs
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

data SingletonStatus
    = NonSingleton       -- separate state for each instance of the script
    | SingletonReference -- this script holds a reference to a "master" script instance that all share
    | SingletonBase      -- this is the master instance
    deriving (Show, Eq)

data ActiveScript = ActiveScript
    { activeState       :: !(MVar Lua.State)
    , eventFlags        :: !EventFlag
    , instanceEntity    :: !Entity
    , watchedVariables  :: !(Vector WatchedVariable)
    , singletonStatus   :: !SingletonStatus
    }

instance Component ActiveScript where type Storage ActiveScript = Map ActiveScript

newtype ScriptBank = ScriptBank {unScriptBank :: IDHashTable ActiveScript }

newtype RunningScriptEntity = RunningScriptEntity {runningScript :: Maybe Entity}

-- | A buffer for IO events that scripts generate; these are executed after the script runs to prevent concurrency issues with same script instances
newtype ScriptEventBuffer = ScriptEventBuffer {unBuffer :: [IO ()]}

handleLua :: a -> Lua a -> Lua a
handleLua !def !lua = do
    res <- Lua.try lua
    case res of
        Left exc  -> liftIO (print exc) >> pure def
        Right ret -> pure ret

handleLuaOp :: Lua a -> Lua ()
handleLuaOp !lua = do
    res <- Lua.try lua
    case res of
        Left exc -> liftIO (print exc)
        Right _  -> pure ()

execFor :: forall w. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer]) => ActiveScript -> Lua () -> SystemT w IO ()
execFor (ActiveScript {activeState = !stmvar, instanceEntity = ety}) fn = do
    global $= RunningScriptEntity (Just ety)

    liftIO $ do
        lstate <- takeMVar stmvar

        newSt  <- Lua.runWith lstate (handleLuaOp fn >> Lua.state)

        putMVar stmvar newSt

    ScriptEventBuffer buffer <- get global
    global $= ScriptEventBuffer []
    liftIO $ forM_ buffer id

    global $= RunningScriptEntity Nothing

runFor :: forall w a. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer]) => Lua.LuaCallFunc a => ActiveScript -> Lua a -> SystemT w IO (Maybe a)
runFor (ActiveScript {activeState = !stmvar, instanceEntity = ety}) fn = do
    global $= RunningScriptEntity (Just ety)

    a <- liftIO $ do
        lstate     <- takeMVar stmvar

        (a, newSt) <- Lua.runWith lstate ((,) <$> handleLua Nothing (Just <$> fn) <*> Lua.state)

        putMVar stmvar newSt
        return a

    ScriptEventBuffer buffer <- get global
    liftIO $ forM_ buffer id
    global $= ScriptEventBuffer []

    global $= RunningScriptEntity Nothing
    return a
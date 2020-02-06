{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- Meant to be imported qualified

module Kathu.Scripting.Lua
    ( module Kathu.Scripting.ExternalFunctions
    , module Kathu.Scripting.Lua.Types
    , call
    , loadScript
    , shouldScriptRun
    , releaseActiveScript
    , execFor
    , runFor
    , mkScriptBank
    ) where

import           Apecs
import           Apecs.Physics                 (Force, Mass, Physics, Position, Velocity)
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.ST              (stToIO)
import qualified Data.HashTable.ST.Basic       as HT
import           Foreign.Lua                   hiding (call, error, runWith)
import qualified Foreign.Lua.Core              as Lua
import qualified Foreign.Lua.FunctionCalling   as Lua

import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Entity.Time
import           Kathu.Graphics.Camera
import           Kathu.Scripting.Event
import           Kathu.Scripting.ExternalFunctions
import           Kathu.Scripting.Lua.Component
import           Kathu.Scripting.Lua.Global
import           Kathu.Scripting.Lua.Types
import           Kathu.Scripting.Variables
import           Kathu.Util.Apecs

logLua :: String -> Lua ()
logLua str = do
    -- in future an actual log file should be used and acessed through the world
    liftIO . putStrLn $ str

call :: Lua.LuaCallFunc a => String -> a
call = callFunc

shouldScriptRun :: ScriptEvent -> ActiveScript -> Bool
shouldScriptRun e (ActiveScript _ f) = isEventSet e f

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

mkActiveScript :: forall w. (ReadWrite w IO ScriptBank) => Lua () -> Script -> SystemT w IO ActiveScript
mkActiveScript !initLua (Script _ mainScr flags _) = (flip ActiveScript) flags <$> liftIO stateMVar
    where !stateMVar = do
              st  <- Lua.newstate
              -- for some reason Lua exception logging doesn't actually get caught here, unlike in execFor...
              st' <- Lua.runWith st (initLua >> handleLuaOp (Lua.dostring mainScr) >> Lua.state)
              newMVar st'

releaseActiveScript :: ActiveScript -> IO ()
releaseActiveScript (ActiveScript stmvar _) = do
    lstate <- takeMVar stmvar
    Lua.close lstate
    putMVar stmvar $ error "Attempted to use a release ActiveScript"

execFor :: ActiveScript -> Lua () -> IO ()
execFor (ActiveScript !stmvar _) fn = do
    lstate <- takeMVar stmvar
    
    newSt  <- Lua.runWith lstate (handleLuaOp fn >> Lua.state)

    putMVar stmvar newSt

runFor :: Lua.LuaCallFunc a => ActiveScript -> Lua a -> IO (Maybe a)
runFor (ActiveScript !stmvar _) fn = do
    lstate     <- takeMVar stmvar

    (a, newSt) <- Lua.runWith lstate ((,) <$> handleLua Nothing (Just <$> fn) <*> Lua.state)

    putMVar stmvar newSt
    pure a

loadScript :: forall w g. (Has w IO Physics, ReadWriteEach w IO '[Camera, Debug, Force, Identity, Local, LogicTime, Mass, MovingSpeed, Position, Random, RenderTime, ScriptBank, Tags, Variables, Velocity])
           => ExternalFunctions w g -> Script -> SystemT w IO ActiveScript
loadScript extFuns script 
    | isSingleton script = fromBank
    | otherwise          = mkAS
    where mkAS = ask >>= (flip mkActiveScript) script . initLua
          initLua world = do
              openbase
              openmath
              openpackage
              openstring
              opentable
              registerHaskellFunction "log" logLua
              registerComponentFunctions world
              registerGlobalFunctions world extFuns
          fromBank = do
              let sID = scriptID script
              sbank     <- unScriptBank <$> get global
              curScript <- liftIO . stToIO $ HT.lookup sbank sID
              case curScript of
                  Just as -> return as
                  Nothing -> do ascript <- mkAS
                                liftIO . stToIO $ HT.insert sbank sID ascript
                                return ascript

mkScriptBank :: IO ScriptBank
mkScriptBank = ScriptBank <$> stToIO (HT.newSized 64)
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- Meant to be imported qualified

module Kathu.Scripting.Lua
    ( module Kathu.Scripting.Lua.Types
    , call
    , loadScript
    , shouldScriptRun
    , releaseActiveScript
    , execFor
    , runFor
    ) where

import           Apecs
import           Apecs.Physics                 (Force, Mass, Physics, Position, Velocity)
import           Control.Concurrent.MVar
import           Control.Exception             (handle)
import           Control.Monad.IO.Class        (liftIO)
import           Foreign.Lua                   hiding (call, error, runWith)
import qualified Foreign.Lua.Core              as Lua
import qualified Foreign.Lua.FunctionCalling   as Lua

import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Entity.Time
import           Kathu.Graphics.Camera
import           Kathu.Scripting.Event
import           Kathu.Scripting.Lua.Component
import           Kathu.Scripting.Lua.Global
import           Kathu.Scripting.Lua.Types
import           Kathu.Util.Apecs

logLua :: String -> Lua ()
logLua str = do
    -- in future an actual log file should be used and acessed through the world
    liftIO . putStrLn $ str

call :: Lua.LuaCallFunc a => String -> a
call = callFunc

shouldScriptRun :: ScriptEvent -> ActiveScript -> Bool
shouldScriptRun e (ActiveScript _ f) = isEventSet e f

onLuaException :: IO a -> Exception -> IO a
onLuaException s e = putStrLn (show e) >> s

mkActiveScript :: Lua () -> Script -> IO ActiveScript
mkActiveScript initLua (Script mainScr flags) = (flip ActiveScript) flags <$> stateMVar
    where stateMVar = do
              st  <- Lua.newstate
              st' <- handle (onLuaException Lua.newstate) $ Lua.runWith st (initLua >> Lua.dostring mainScr >> Lua.state)
              newMVar st'

releaseActiveScript :: ActiveScript -> IO ()
releaseActiveScript (ActiveScript stmvar _) = do
    lstate <- takeMVar stmvar
    Lua.close lstate
    putMVar stmvar $ error "Attempted to use a release ActiveScript"

execFor :: ActiveScript -> Lua () -> IO ()
execFor (ActiveScript stmvar _) fn = do
    lstate <- takeMVar stmvar
    
    newSt  <- handle (onLuaException Lua.newstate) $ Lua.runWith lstate (fn >> Lua.state)

    putMVar stmvar newSt

runFor :: Lua.LuaCallFunc a => ActiveScript -> Lua a -> IO (Maybe a)
runFor (ActiveScript stmvar _) fn = do
    lstate     <- takeMVar stmvar

    let runL = Lua.runWith lstate ((,) <$> (Just <$> fn) <*> Lua.state)

    (a, newSt) <- handle (onLuaException ((Nothing,) <$> Lua.newstate)) runL

    putMVar stmvar newSt
    pure a

loadScript :: forall w. (Has w IO Physics, ReadWriteEach w IO '[Camera, Force, Identity, Local, LogicTime, Mass, MovingSpeed, Position, Random, RenderTime, Tags, Velocity])
           => ExternalFunctions w -> Script -> SystemT w IO ActiveScript
loadScript extFuns script = ask >>= liftIO . (flip mkActiveScript) script . initLua
    where initLua world = do
              openbase
              openmath
              openpackage
              openstring
              opentable
              registerHaskellFunction "log" logLua
              registerComponentFunctions world
              registerGlobalFunctions world extFuns
              pure ()

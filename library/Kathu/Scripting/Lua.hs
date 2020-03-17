{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

-- Meant to be imported qualified

module Kathu.Scripting.Lua
    ( module Kathu.Scripting.ExternalFunctions
    , module Kathu.Scripting.Lua.Types
    , HasScripting
    , call
    , loadScript
    , shouldScriptRun
    , addWireController
    , addWireReceiver
    , releaseActiveScript
    , initScripting
    ) where

import           Apecs
import           Apecs.Physics                     (Force, Mass, Physics, Position, Velocity)
import           Control.Concurrent.MVar
import           Control.Monad                     (unless, when)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.ST                  (stToIO)
import qualified Data.HashTable.ST.Basic           as HT
import qualified Data.Vector                       as Vec
import           Foreign.Lua                       hiding (call, error, runWith)
import qualified Foreign.Lua.Core                  as Lua
import qualified Foreign.Lua.FunctionCalling       as Lua

import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Entity.Time
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Drawable           (Render)
import           Kathu.Scripting.Event
import           Kathu.Scripting.ExternalFunctions
import           Kathu.Scripting.Lua.Component
import           Kathu.Scripting.Lua.Global
import           Kathu.Scripting.Lua.Types
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Kathu.Util.Apecs
import           Kathu.Util.Types

type HasScripting w m = ReadWriteEach w m [ActiveScript, RunningScriptEntity, ScriptBank, ScriptEventBuffer, WireReceivers]

-- in future an actual log file should be used and acessed through the world
logLua :: String -> Lua ()
logLua = liftIO . putStrLn

call :: Lua.LuaCallFunc a => String -> a
call = callFunc

shouldScriptRun :: ScriptEvent -> ActiveScript -> Bool
shouldScriptRun e = isEventSet e . eventFlags

addWireController :: Identifier -> ActiveScript -> ActiveScript
addWireController sigName as = as { wireSignals = Vec.cons sigName (wireSignals as) }

addWireReceiver :: forall w. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer, WireReceivers])
                => Identifier -> ActiveScript -> SystemT w IO ()
addWireReceiver sigName as@ActiveScript {instanceEntity = ety} = do
    world     <- ask
    receivers <- get global

    let onChange     = execFor as . call "onSignalChange" (unEntity ety)
        onChangeIO i = Apecs.runWith world $ do
            ScriptEventBuffer buffer <- get global
            global $= ScriptEventBuffer (Apecs.runWith world (onChange i) : buffer)
            
    liftIO $ addWireListener sigName (unEntity ety) onChangeIO receivers

mkActiveScript :: forall w. (ReadWrite w IO ScriptBank) => Entity -> SingletonStatus -> Lua () -> Script -> SystemT w IO ActiveScript
mkActiveScript !ety !singStatus !initLua (Script _ mainScr flags _) = mkActive <$> liftIO stateMVar
    where mkActive st = ActiveScript st flags ety Vec.empty Vec.empty singStatus
          !stateMVar = do
              st  <- Lua.newstate
              -- for some reason Lua exception logging doesn't actually get caught here, unlike in execFor...
              st' <- Lua.runWith st (initLua >> handleLuaOp (Lua.dostring mainScr) >> Lua.state)
              newMVar st'

releaseActiveScript :: forall w. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer, Variables, WireReceivers]) => ActiveScript -> SystemT w IO ()
releaseActiveScript as@(ActiveScript stmvar _ scriptEntity watched signals singStatus) = do
    let ety = unEntity scriptEntity
    when (scriptEntity /= global && shouldScriptRun onDestroy as) $
        execFor as (call "onDestroy" ety)

    receivers <- get global
    Vec.forM_ signals $ deleteWireListener ety receivers

    -- don't want to fully release an singleton instance at this time if we aren't clearing the master, non-shared script state
    when (singStatus /= SingletonReference) $ do
        unless (Vec.null watched) $ do
            vars <- get global
            Vec.forM_ watched $ deleteListener ety vars

        liftIO $ do
            lstate <- takeMVar stmvar
            Lua.close lstate
            putMVar stmvar $ error "Attempted to use a release ActiveScript"

loadScript :: forall w g. (Has w IO Physics, Members w IO (Render g), ReadWriteEach w IO [ActiveScript, Camera, Debug, Force, Identity, Local, LogicTime, Mass, MovingSpeed, Position, Random, Render g, RenderTime, RunningScriptEntity, ScriptBank, ScriptEventBuffer, Tags, Variables, Velocity, WireReceivers])
           => ExternalFunctions w g -> Entity -> Script -> SystemT w IO ActiveScript
loadScript extFuns ety script
    | isSingleton script = runIfOnInit =<< fromBank
    | otherwise          = runIfOnInit =<< mkAS NonSingleton ety
    where mkAS singStatus e = ask >>= (\l -> mkActiveScript e singStatus l script) . initLua
          initLua world = do
              openbase
              openmath
              openpackage
              openstring
              opentable
              registerHaskellFunction "log" logLua
              registerComponentFunctions world extFuns
              registerGlobalFunctions world extFuns
          runIfOnInit as | shouldScriptRun onInit as = set ety as >> execFor as (call "onInit" (unEntity ety)) >> return as
                         | otherwise                 = set ety as >> return as
          fromBank = do
              let sID = scriptID script
              sbank     <- unScriptBank <$> get global
              curScript <- liftIO . stToIO $ HT.lookup sbank sID
              case curScript of
                  Just as -> return $ as {instanceEntity = ety, singletonStatus = SingletonReference}
                  Nothing -> do ascript <- mkAS SingletonBase global
                                liftIO . stToIO $ HT.insert sbank sID ascript
                                return $ ascript {instanceEntity = ety, singletonStatus = SingletonReference}

initScripting :: forall w. (ReadWriteEach w IO [ScriptBank, WireReceivers]) => SystemT w IO ()
initScripting = do
    scriptBank <- liftIO $ ScriptBank <$> stToIO (HT.newSized 64)
    receivers  <- liftIO $ WireReceivers <$> stToIO (HT.newSized 64)
    global $= scriptBank
    global $= receivers
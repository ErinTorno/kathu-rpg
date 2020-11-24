-- Meant to be imported qualified
module Kathu.Scripting.Lua
    ( module Kathu.Scripting.Lua.Types
    , call
    , loadScript
    , addWireController
    , addWireReceiver
    , releaseActiveScript
    , initScripting
    ) where

import           Apecs
import           Control.Concurrent.MVar
import           Control.Monad                     (forM_, unless, when)
import           Control.Monad.ST                  (stToIO)
import           Control.Lens                      ((^.))
import qualified Data.HashTable.ST.Basic           as HT
import qualified Data.Map                          as Map
import qualified Data.Vector                       as Vec
import           Foreign.Lua                       hiding (call, error)
import qualified Foreign.Lua.Core                  as Lua
import qualified Foreign.Lua.FunctionCalling       as Lua

import           Kathu.Scripting.Event
import           Kathu.Scripting.Lua.Types
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Verda.Util.Apecs

type HasScripting w m = ReadWriteEach w m [ActiveScript, LuaModules, RunningScriptEntity, ScriptBank, ScriptEventBuffer, Variables, WireReceivers]

call :: Lua.LuaCallFunc a => String -> a
call = callFunc

releaseActiveScript :: HasScripting w IO => ActiveScript -> SystemT w IO ()
releaseActiveScript as@(ActiveScript stmvar _ scriptEntity watched _ wireReceivers singStatus _) = do
    let ety = unEntity scriptEntity
    when (scriptEntity /= global && shouldScriptRun onDestroy as) $
        execFor as (call "onDestroy" ety)

    receivers <- get global
    Vec.forM_ wireReceivers $ deleteWireListener ety receivers

    -- don't want to fully release an singleton instance at this time if we aren't clearing the master, non-shared script state
    when (singStatus /= SingletonReference) $ do
        unless (Vec.null watched) $ do
            vars <- get global
            Vec.forM_ watched $ deleteListener ety vars

        liftIO $ do
            lstate <- takeMVar stmvar
            Lua.close lstate
            putMVar stmvar $ error "Attempted to use a release ActiveScript"

mkActiveScript :: HasScripting w IO => (ActiveScript -> ActiveScript) -> Entity -> SingletonStatus -> Lua () -> Script -> SystemT w IO ActiveScript
mkActiveScript mapper !ety !singStatus !initLua (Script _ mainScr flags _) = do
    mvar <- liftIO newEmptyMVar
    -- warning: by this point no mvar is supplied, so if something tries to use it it will have issues
    -- fortunately nothing should, as this won't trigger execFor/runFor or events
    let baseAS = mapper $ ActiveScript mvar flags ety Vec.empty Vec.empty Vec.empty singStatus Map.empty
    ety    $= baseAS
    global $= RunningScriptEntity (Just ety)

    mvar' <- liftIO $ do
        st  <- Lua.newstate
        -- for some reason Lua exception logging doesn't actually get caught here, unlike in execFor...
        st' <- Foreign.Lua.runWith st (initLua >> handleLuaOp (Lua.dostring mainScr) >> Lua.state)
        newMVar st'
    pure $ baseAS {activeState = mvar'}

loadScript :: HasScripting w IO => (ActiveScript -> ActiveScript) -> Entity -> Script -> SystemT w IO ActiveScript
loadScript mapper ety script
    | script^.isSingleton = runIfOnInit =<< fromBank
    | otherwise           = runIfOnInit =<< mkAS mapper NonSingleton ety
    where mkAS f singStatus e = (\l -> mkActiveScript f e singStatus l script) . initLua =<< get global
          initLua (LuaModules modules) = do
              openbase
              openmath
              openpackage
              openstring
              opentable
              forM_ modules id
          runIfOnInit as | shouldScriptRun onInit as = set ety as >> execFor as (call "onInit" (unEntity ety)) >> return as
                         | otherwise                 = set ety as >> return as
          fromBank = do
              let sID = script^.scriptID
              sbank     <- unScriptBank <$> get global
              curScript <- liftIO . stToIO $ HT.lookup sbank sID
              case curScript of
                  Just as -> pure . mapper $ as {instanceEntity = ety, singletonStatus = SingletonReference}
                  Nothing -> do ascript <- mkAS id SingletonBase global
                                liftIO . stToIO $ HT.insert sbank sID ascript
                                pure . mapper $ ascript {instanceEntity = ety, singletonStatus = SingletonReference}

initScripting :: HasScripting w IO => SystemT w IO ()
initScripting = do
    scriptBank <- liftIO $ ScriptBank <$> stToIO (HT.newSized 64)
    receivers  <- liftIO $ WireReceivers <$> stToIO (HT.newSized 64)
    global $= scriptBank
    global $= receivers
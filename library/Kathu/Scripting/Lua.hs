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
    , mkScriptBank
    ) where

import           Apecs
import           Apecs.Physics                     (Force, Mass, Physics, Position, Velocity)
import           Control.Concurrent.MVar
import           Control.Monad                     (when)
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
import           Kathu.Util.Apecs

logLua :: String -> Lua ()
logLua str = do
    -- in future an actual log file should be used and acessed through the world
    liftIO . putStrLn $ str

call :: Lua.LuaCallFunc a => String -> a
call = callFunc

shouldScriptRun :: ScriptEvent -> ActiveScript -> Bool
shouldScriptRun e = isEventSet e . eventFlags

mkActiveScript :: forall w. (ReadWrite w IO ScriptBank) => Entity -> SingletonStatus -> Lua () -> Script -> SystemT w IO ActiveScript
mkActiveScript !ety !singStatus !initLua (Script _ mainScr flags _) = mkActive <$> liftIO stateMVar
    where mkActive st = ActiveScript st flags ety Vec.empty singStatus
          !stateMVar = do
              st  <- Lua.newstate
              -- for some reason Lua exception logging doesn't actually get caught here, unlike in execFor...
              st' <- Lua.runWith st (initLua >> handleLuaOp (Lua.dostring mainScr) >> Lua.state)
              newMVar st'

releaseActiveScript :: forall w. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer, Variables]) => ActiveScript -> SystemT w IO ()
releaseActiveScript as@(ActiveScript stmvar _ scriptEntity watched singStatus) = do
    let ety = unEntity scriptEntity
    when (scriptEntity /= global && shouldScriptRun onDestroy as) $ do
        execFor as (call "onDestroy" ety)

    -- don't want to fully release an singleton instance at this time if we aren't clearing the master, non-shared script state
    when (singStatus /= SingletonReference) $ do
        when (not $ Vec.null watched) $ do
            vars <- get global
            Vec.forM_ watched $ deleteListener ety vars

        liftIO $ do
            lstate <- takeMVar stmvar
            Lua.close lstate
            putMVar stmvar $ error "Attempted to use a release ActiveScript"

loadScript :: forall w g. (Has w IO Physics, Members w IO (Render g), ReadWriteEach w IO [ActiveScript, Camera, Debug, Force, Identity, Local, LogicTime, Mass, MovingSpeed, Position, Random, Render g, RenderTime, RunningScriptEntity, ScriptBank, ScriptEventBuffer, Tags, Variables, Velocity])
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
                  Just as -> do return $ as {instanceEntity = ety, singletonStatus = SingletonReference}
                  Nothing -> do ascript <- mkAS SingletonBase global
                                liftIO . stToIO $ HT.insert sbank sID ascript
                                return $ ascript {instanceEntity = ety, singletonStatus = SingletonReference}

mkScriptBank :: IO ScriptBank
mkScriptBank = ScriptBank <$> stToIO (HT.newSized 64)
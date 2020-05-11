-- Meant to be imported qualified
module Kathu.Scripting.Lua
    ( module Kathu.Scripting.ExternalFunctions
    , module Kathu.Scripting.Lua.Types
    , HasScripting
    , call
    , loadScript
    , shouldScriptRun
    , setInstanceConfig
    , addWireController
    , addWireReceiver
    , releaseActiveScript
    , initScripting
    ) where

import           Apecs
import           Apecs.Physics                     (Force, Mass, Physics, Position, Velocity)
import           Control.Concurrent.MVar
import           Control.Monad                     (unless, when)
import           Control.Monad.ST                  (stToIO)
import           Control.Lens                      ((^.))
import qualified Data.HashTable.ST.Basic           as HT
import qualified Data.Map                          as Map
import qualified Data.Vector                       as Vec
import           Foreign.Lua                       hiding (call, error, runWith)
import qualified Foreign.Lua.Core                  as Lua
import qualified Foreign.Lua.FunctionCalling       as Lua

import           Kathu.Entity.Components
import           Kathu.Entity.Cursor
import           Kathu.Entity.Logger
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

call :: Lua.LuaCallFunc a => String -> a
call = callFunc

shouldScriptRun :: ScriptEvent -> ActiveScript -> Bool
shouldScriptRun e = isEventSet e . eventFlags

setInstanceConfig :: IDMap WorldVariable -> ActiveScript -> ActiveScript
setInstanceConfig config script = script {instanceConfig = config}

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

mkActiveScript :: forall w. (HasScripting w IO) => (ActiveScript -> ActiveScript) -> Entity -> SingletonStatus -> Lua () -> Script -> SystemT w IO ActiveScript
mkActiveScript mapper !ety !singStatus !initLua (Script _ mainScr flags _) = do
    mvar <- liftIO newEmptyMVar
    -- warning: by this point no mvar is supplied, so if something tries to use it it will have issues
    -- fortunately nothing should, as this won't trigger execFor/runFor or events
    let baseAS = mapper $ ActiveScript mvar flags ety Vec.empty Vec.empty singStatus Map.empty
    ety    $= baseAS
    global $= RunningScriptEntity (Just ety)

    mvar' <- liftIO $ do
        st  <- Lua.newstate
        -- for some reason Lua exception logging doesn't actually get caught here, unlike in execFor...
        st' <- Lua.runWith st (initLua >> handleLuaOp (Lua.dostring mainScr) >> Lua.state)
        newMVar st'
    pure $ baseAS {activeState = mvar'}

releaseActiveScript :: forall w. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer, Variables, WireReceivers]) => ActiveScript -> SystemT w IO ()
releaseActiveScript as@(ActiveScript stmvar _ scriptEntity watched signals singStatus _) = do
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

loadScript :: forall w g. (Has w IO Physics, Members w IO (Render g), ReadWriteEach w IO [ActiveScript, Camera, CursorMotionState, Debug, Force, Identity, Local, Logger, LogicTime, Mass, MovingSpeed, Position, Random, Render g, RenderTime, RunningScriptEntity, ScriptBank, ScriptEventBuffer, Tags, Variables, Velocity, WireReceivers])
           => (ActiveScript -> ActiveScript) -> ExternalFunctions w g -> Entity -> Script -> SystemT w IO ActiveScript
loadScript mapper extFuns ety script
    | script^.isSingleton = runIfOnInit =<< fromBank
    | otherwise           = runIfOnInit =<< mkAS mapper NonSingleton ety
    where mkAS f singStatus e = ask >>= (\l -> mkActiveScript f e singStatus l script) . initLua
          initLua world = do
              openbase
              openmath
              openpackage
              openstring
              opentable
              registerComponentFunctions world extFuns
              registerGlobalFunctions world extFuns
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

initScripting :: forall w. (ReadWriteEach w IO [ScriptBank, WireReceivers]) => SystemT w IO ()
initScripting = do
    scriptBank <- liftIO $ ScriptBank <$> stToIO (HT.newSized 64)
    receivers  <- liftIO $ WireReceivers <$> stToIO (HT.newSized 64)
    global $= scriptBank
    global $= receivers
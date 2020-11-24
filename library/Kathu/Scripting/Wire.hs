module Kathu.Scripting.Wire where

import           Apecs
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.ST          (stToIO)
import qualified Data.HashTable.ST.Basic   as HT
import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IntMap
import qualified Data.Vector               as Vec
import qualified Foreign.Lua               as Lua
import           Verda.Util.Apecs
import           Verda.Util.Types

import           Kathu.Scripting.Lua.Types

type OnSignalChange = Int -> IO ()

data WireReceiver = WireReceiver
    { wirePower  :: !Int
    , wireEvents :: !(IntMap OnSignalChange)
    }

newtype WireReceivers = WireReceivers {unWireConnections :: IDHashTable WireReceiver}

instance Semigroup WireReceivers where (<>) = mappend
instance Monoid WireReceivers where mempty = error "Attempted to use WireReceivers before it has been loaded"
instance Component WireReceivers where type Storage WireReceivers = Global WireReceivers

addWireListener :: MonadIO m => Identifier -> Int -> OnSignalChange -> WireReceivers -> m ()
addWireListener sigName iid onChange (WireReceivers con) = liftIO . stToIO $ HT.mutate con sigName ((,()) . addL)
    where addL Nothing         = Just $ WireReceiver 0 (IntMap.singleton iid onChange)
          addL (Just receiver) = Just $ receiver {wireEvents = IntMap.insert iid onChange (wireEvents receiver)}

deleteWireListener :: MonadIO m => Int -> WireReceivers -> Identifier -> m ()
deleteWireListener iid (WireReceivers con) sigName = liftIO . stToIO $ HT.mutate con sigName ((,()) . addL)
    where addL Nothing         = Nothing
          addL (Just receiver) = Just $ receiver {wireEvents = IntMap.delete iid (wireEvents receiver)}

mutateWirePower :: MonadIO m => Identifier -> (Int -> Int) -> WireReceivers -> m ()
mutateWirePower sigName f (WireReceivers con) = do

    maybeReceiver <- liftIO . stToIO $ HT.lookup con sigName

    forM_ maybeReceiver $ \receiver -> do
        let power' = f (wirePower receiver)

        liftIO . stToIO . HT.insert con sigName $ receiver {wirePower = power'}

        liftIO $ forM_ (wireEvents receiver) ($power')

------------
-- Script --
------------

addWireController :: Identifier -> ActiveScript -> ActiveScript
addWireController sigName as = as {wireControllers = Vec.cons sigName (wireControllers as)}

addWireReceiver :: forall w. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer, WireReceivers])
                => Identifier -> ActiveScript -> SystemT w IO ActiveScript
addWireReceiver sigName as@ActiveScript {instanceEntity = ety} = do
    world     <- ask
    receivers <- get global

    let onChange     = execFor as . Lua.callFunc "onSignalChange" (unEntity ety)
        onChangeIO i = Apecs.runWith world $ do
            ScriptEventBuffer buffer <- get global
            global $= ScriptEventBuffer (Apecs.runWith world (onChange i) : buffer)
            
    liftIO $ addWireListener sigName (unEntity ety) onChangeIO receivers
    pure $ as {wireReceivers = Vec.cons sigName (wireReceivers as)}
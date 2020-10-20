module Kathu.Scripting.Wire where

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.ST        (stToIO)
import qualified Data.HashTable.ST.Basic as HT
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap

import           Verda.Util.Types

type OnSignalChange = Int -> IO ()

data WireReceiver = WireReceiver
    { wirePower  :: !Int
    , wireEvents :: !(IntMap OnSignalChange)
    }

newtype WireReceivers = WireReceivers {unWireConnections :: IDHashTable WireReceiver}

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
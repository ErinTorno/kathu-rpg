module Kathu.Parsing.Counting
    ( CountingIDs(..)
    , parseAndLookupOrAddIncrementalID
    , lookupOrAdd
    , lookupOrExecAndVerify
    ) where

import           Control.Monad         (when)
import           Data.Aeson
import           Data.Aeson.Types      (Parser)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Kathu.Util.Dependency

newtype CountingIDs = CountingIDs {unCounting :: Map Text (Map Text Int)}

-- | Takes a type name and a constructor that takes an Integral, and returns a parser that will yield the ID
-- | If the ID has already been read, that is returned; otherwise the ID is inserted into the map, and the new ID is returned
parseAndLookupOrAddIncrementalID :: (Integral a, s `CanStore` CountingIDs, Monad m) => (a -> b) -> Text -> Value -> Parser (Dependency s m b)
parseAndLookupOrAddIncrementalID constructor category = withText (T.unpack category) $ \s ->
    pure (constructor . fromIntegral <$> lookupOrAdd category s)

lookupOrAdd :: (s `CanStore` CountingIDs, Monad m) => Text -> Text -> Dependency s m Int
lookupOrAdd = lookupOrExecAndAdd $ \_ -> pure ()

-- | Will look up the stored number for the keys; if its not found, it will run the given action and verify the result matches then next ID to store
lookupOrExecAndVerify :: (s `CanStore` CountingIDs, Monad m) => Dependency s m Int -> Text -> Text -> Dependency s m Int
lookupOrExecAndVerify action = lookupOrExecAndAdd $ \numIDMap -> do
    nextID <- action
    when (nextID /= Map.size numIDMap) $
        error . concat $ ["Result from executing (", show nextID, ") did not match the next expected ID (", show (Map.size numIDMap), ")"]

lookupOrExecAndAdd :: (s `CanStore` CountingIDs, Monad m) => (Map Text Int -> Dependency s m ()) -> Text -> Text -> Dependency s m Int
lookupOrExecAndAdd action category key = do
    CountingIDs countingIDs <- readStore
    let numIDMap = Map.findWithDefault Map.empty category countingIDs

    case Map.lookup key numIDMap of
        Just i  -> pure i
        Nothing -> do
            action numIDMap
            let nextID    = Map.size numIDMap
                numIDMap' = Map.insert key nextID numIDMap
            writeStore . CountingIDs . Map.insert category numIDMap' $ countingIDs
            pure nextID
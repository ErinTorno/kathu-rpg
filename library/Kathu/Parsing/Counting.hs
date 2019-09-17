{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Kathu.Parsing.Counting where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Kathu.Util.Dependency

newtype CountingIDs = CountingIDs {unCounting :: Map Text (Map Text Int)}

lookupOrAdd :: (s `CanStore` CountingIDs, Monad m) => Text -> Text -> Dependency s m Int
lookupOrAdd = lookupOrExecAndVerify (pure Nothing)

-- | Will look up the stored number for the keys; if its not found, it will run the given action and verify the result matches then next ID to store
lookupOrExecAndVerify :: (s `CanStore` CountingIDs, Monad m) => Dependency s m (Maybe Int) -> Text -> Text -> Dependency s m Int
lookupOrExecAndVerify action category key = (getID <$> readStore) >>= storeIfNeeded False
    where initialMap                   = Map.insert key 0 Map.empty
          getID (CountingIDs ids)      = Map.lookup category ids >>= Map.lookup key
          checkResult (Just i) nextID  = if i == nextID then i else error . concat $ ["Result from executing (", show i, ") did not match the next expected ID (", show nextID, ")"]
          checkResult Nothing nextID   = nextID
          storeIfNeeded _ (Just i)     = pure i
          storeIfNeeded False Nothing  = action
                                     >>= \result -> (unCounting <$> readStore)
                                     >>= writeStore . CountingIDs . Map.insertWith (\_ old -> Map.insert key (checkResult result . Map.size $ old) old) category initialMap
                                      >> (getID <$> readStore)
                                     >>= storeIfNeeded True
          storeIfNeeded True Nothing   = error . concat $
              [ "lookupOrAdd failed to insert the new key "
              , show key
              , " with category "
              , show category
              , "; this should never happen, and marks some error in the function's logic, rather than input failure"
              ]
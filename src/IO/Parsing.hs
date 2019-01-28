{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Parsing where

import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Functor
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import qualified SDL
import System.FilePath

import IO.File

fromScientific :: Fractional a => Scientific -> a
fromScientific = fromRational . toRational

-- Helpers for desparseSLerialization that works with state

data ParsingLibrary = ParsingLibrary
    { images :: Map Text SDL.Surface
    , tileIDs :: Map Text Int
    , toolIDs :: Map Text Int -- tool types are stored as strings when serialized, but as integers when loaded
    , workingDirectory :: String
    }

emptyPL = ParsingLibrary
    { images = Map.empty
    , tileIDs = Map.empty
    , toolIDs = Map.empty
    , workingDirectory = ""
    }

newtype SystemLink a = SystemLink (StateT ParsingLibrary IO a) deriving (Functor, Applicative, Monad, MonadState ParsingLibrary)

resultList (l, _) = l
resultLib (_, lb) = lb

runSL :: ParsingLibrary -> SystemLink a -> IO (a, ParsingLibrary)
runSL lib (SystemLink st) = runStateT st lib

foldSL :: ParsingLibrary -> [SystemLink a] -> IO ([a], ParsingLibrary)
foldSL lib = foldM (\(!v, !lib) cur -> (\(nv, nlib) -> (nv:v, nlib)) <$> runSL lib cur) ([], lib)

parseSL :: ParsingLibrary -> IO [SystemLink a] -> IO ([a], ParsingLibrary)
parseSL lib ls = ls >>= foldSL lib

showWD :: SystemLink ()
showWD = gets workingDirectory >>= pure . error

loadFromFileSL :: FromJSON (SystemLink a) => FilePath -> IO (SystemLink a)
loadFromFileSL file = loadWithHandlers (loadError file) (modWDir>>) file
    where modWDir :: SystemLink ()
          modWDir = modify updateWDir
          updateWDir pl = pl {workingDirectory = takeDirectory file}

parseAllSL :: FromJSON (SystemLink a) => String -> FilePath -> IO [SystemLink a]
parseAllSL = parseAllWith loadFromFileSL

-- instance MonadState SystemLink where

liftSL :: IO a -> SystemLink a
liftSL = SystemLink . lift

-- gets a normal type from Parser and raises it to a Compose
(.:^) :: (Monad m, FromJSON a) => Object -> Text -> Compose Parser m a
(.:^) v = Compose . liftM return . (.:) v

-- gets a type encased in a monad from a Parser and Composes it
(.:~) :: (Monad m, FromJSON (m a)) => Object -> Text -> Compose Parser m a
(.:~) v = Compose . (.:) v

-- same as .:^, but returns a Maybe
(.:^?) :: (Monad m, FromJSON a) => Object -> Text -> Compose Parser m (Maybe a)
(.:^?) v = Compose . liftM return . (.:?) v

-- same as .:~, but returns a Maybe
(.:~?) :: (Monad m, FromJSON (m a)) => Object -> Text -> Compose Parser m (Maybe a)
(.:~?) v t = Compose (nestedChange <$> v .:? t)
    where nestedChange = maybe (return Nothing) (\v -> v >>= return . Just)

parseUrl :: FilePath -> SystemLink String
parseUrl path = gets workingDirectory >>= \dir -> pure . resolveAssetPath dir $ path

lookupOrAdd :: (ParsingLibrary -> Map Text a) -> (ParsingLibrary -> ParsingLibrary) -> Text -> SystemLink a
lookupOrAdd getMap addVal key = (Map.lookup key <$> gets getMap) >>= check
    where check (Just v) = pure v
          check Nothing  = modify addVal >> lookupOrAdd getMap addVal key

mapInsertIncr :: Ord a => a -> Map a Int -> Map a Int
mapInsertIncr k m = Map.insert k (Map.size m + 1) m
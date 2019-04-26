{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes, TemplateHaskell #-}

module Kathu.IO.Parsing where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types (Parser, Value, typeMismatch)
import Data.Functor
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import Kathu.Entity.Item (Item)
import Kathu.Graphics.Drawable
import Kathu.IO.File
import qualified SDL
import System.FilePath

projectOptions = defaultOptions {fieldLabelModifier = camelTo2 '-', omitNothingFields = True}

fromScientific :: Fractional a => Scientific -> a
fromScientific = fromRational . toRational

-- Helpers for desparseSLerialization that works with state

data ParsingLibrary = ParsingLibrary
    { _images :: Map Text Image
    , _countingIDs :: Map Text (Map Text Int) -- First key is category, second is individual id and associated index
    , _items :: Map Text Item
    , _workingDirectory :: String
    , _renderer :: SDL.Renderer
    }
makeLenses ''ParsingLibrary

mkEmptyPL :: SDL.Renderer -> ParsingLibrary
mkEmptyPL renderer = ParsingLibrary
    { _images = Map.empty
    , _countingIDs = Map.empty
    , _items = Map.empty
    , _workingDirectory = ""
    , _renderer = renderer
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
showWD = gets (view workingDirectory) >>= pure . error

loadFromFileSL :: FromJSON (SystemLink a) => FilePath -> IO (SystemLink a)
loadFromFileSL file = loadWithHandlers (loadError file) (modWDir>>) file
    where modWDir :: SystemLink ()
          modWDir = modify (set workingDirectory $ takeDirectory file)

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
parseUrl path = gets (view workingDirectory) >>= \dir -> pure . resolveAssetPath dir $ path

lookupOrAdd :: Text -> Text -> (Map Text Int -> Map Text Int) -> SystemLink Int
lookupOrAdd cat key adder = (Map.lookup cat <$> gets (view countingIDs)) >>= checkCat
    where checkCat (Just v) = check . Map.lookup key $ v
          checkCat Nothing  = modify (over countingIDs (Map.insert cat Map.empty)) >> check Nothing          
          check (Just v) = pure v
          check Nothing  = modify addVal >> lookupOrAdd cat key (\_ -> error "Attempting to add value during look up more than once")
          addVal = over countingIDs (Map.adjust adder cat)

mapInsertIncr :: Ord k => k -> Map k Int -> Map k Int
mapInsertIncr k m = Map.insert k (Map.size m + 1) m

incrUpdateIDs :: (Ord t, Ord k) => t -> k -> Map t (Map k Int) -> Map t (Map k Int)
incrUpdateIDs typ k m = Map.adjust updateInner typ newMap
    where updateInner inmap = Map.insert k (Map.size inmap + 1) inmap
          newMap = if Map.member typ m then m else Map.insert typ Map.empty m

lookupEach :: (Ord k, MonadState s m) => Getter s (Map k a) -> [k] -> m [a]
lookupEach getter keys = gets (view getter) >>= \sMap -> pure . foldl (\acc key -> ((Map.!) sMap key): acc) [] $ keys

lookupSingle :: (Ord k, MonadState s m) => Getter s (Map k a) -> k -> m a
lookupSingle getter key = gets (view getter) >>= pure . (flip (Map.!)) key

insertSL :: Ord k => Lens' ParsingLibrary (Map k a) -> k -> a -> SystemLink a
insertSL getter key val = modify (over getter $ Map.insert key val) $> val

parseListSLWith :: (Value -> Parser (SystemLink a)) -> Value -> Parser (SystemLink [a])
parseListSLWith parser (Array a) = foldM append (pure []) a
    where append acc cur = parser cur >>= pure . (flip (liftM2 (:))) acc
parseListSLWith _ v              = typeMismatch "[SystemLink a]" v

parseListSL :: FromJSON (SystemLink a) => Value -> Parser (SystemLink [a])
parseListSL = parseListSLWith parseJSON
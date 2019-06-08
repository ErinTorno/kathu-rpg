{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Kathu.IO.Parsing where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types (Parser, Value, typeMismatch)
import Data.Functor
import Data.Functor.Compose
import qualified Data.HashMap.Strict as Hash
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Data.Scientific
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Kathu.IO.File
import Kathu.Util.Misc (dropInitial, (>>>=))
import System.FilePath

-- We drop the starting _ so that fields for lenses don't keep it
projectOptions :: Options
projectOptions = defaultOptions {fieldLabelModifier = camelTo2 '-' . dropInitial '_', omitNothingFields = True}

fromScientific :: Fractional a => Scientific -> a
fromScientific = fromRational . toRational

-- Helpers for stateful deserialization

newtype SystemLink l a = SystemLink (StateT l IO a) deriving (Functor, Applicative, Monad, MonadState l)

runSL :: l -> SystemLink l a -> IO (a, l)
runSL lib (SystemLink st) = runStateT st lib

foldSL :: l -> [SystemLink l a] -> IO ([a], l)
foldSL initLib = foldM (\(!v, !lib) cur -> (\(nv, lib') -> (nv:v, lib')) <$> runSL lib cur) ([], initLib)

parseSL :: l -> IO [SystemLink l a] -> IO ([a], l)
parseSL lib ls = ls >>= foldSL lib

loadFromFileSL :: FromJSON (SystemLink l a) => Lens' l String -> FilePath -> IO (SystemLink l a)
loadFromFileSL workingDirectory file = loadWithHandlers (loadError file) (modWDir>>) file
    where modWDir = modify (set workingDirectory $ takeDirectory file)

parseAllSL :: FromJSON (SystemLink l a) => Lens' l String -> String -> FilePath -> IO [SystemLink l a]
parseAllSL wd = parseAllWith (loadFromFileSL wd)

parseExactlyNSL :: FromJSON (SystemLink l a) => Lens' l String -> Int -> String -> FilePath -> IO [SystemLink l a]
parseExactlyNSL wd n s = fmap (\ls -> let len = length ls in if len /= n then fail len else ls) . parseAllSL wd s
    where fail len = error . concat $ ["Attempted to parse ", show n, " for file type .", show s, ", but found ", show len]

liftSL :: IO a -> SystemLink l a
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
(.:~?) obj t = Compose (nestedChange <$> obj .:? t)
    where nestedChange = maybe (return Nothing) (\v -> v >>= return . Just)

(.!=~) :: Monad m => Compose Parser m (Maybe a) -> a -> Compose Parser m a
(.!=~) p def = fmap (fromMaybe def) p

parseUrl :: Getter l String -> FilePath -> SystemLink l String
parseUrl workingDirectory path = (flip resolveAssetPath) path <$> gets (view workingDirectory)

lookupOrAddSL :: Lens' l (Map Text (Map Text Int)) -> Text -> Text -> (SystemLink l ()) -> SystemLink l Int
lookupOrAddSL countingIDs cat key adder = (Map.lookup cat <$> gets (view countingIDs)) >>= checkCat
    where checkCat (Just v) = check . Map.lookup key $ v
          checkCat Nothing  = modify (over countingIDs (Map.insert cat Map.empty)) >> check Nothing          
          check (Just v) = pure v
          check Nothing  = adder >> lookupOrAdd countingIDs cat key failure
          failure = pure . error . concat $ ["Attempting to add value during look up more than once (key: ", show key, ", category: ", show cat, ")"]

lookupOrAdd :: Lens' l (Map Text (Map Text Int)) -> Text -> Text -> (Map Text Int -> Map Text Int) -> SystemLink l Int
lookupOrAdd countingIDs cat key adder = lookupOrAddSL countingIDs cat key (modify $ over countingIDs (Map.adjust adder cat))

mapInsertIncr :: Ord k => k -> Map k Int -> Map k Int
mapInsertIncr k m = Map.insert k (Map.size m) m

incrUpdateIDs :: (Ord t, Ord k) => t -> k -> Map t (Map k Int) -> Map t (Map k Int)
incrUpdateIDs typ k m = Map.adjust updateInner typ newMap
    where updateInner inmap = Map.insert k (Map.size inmap + 1) inmap
          newMap = if Map.member typ m then m else Map.insert typ Map.empty m

failWithKeyNotFound :: (Show k) => k -> a
failWithKeyNotFound = error . ("Couldn't find element with key "++) . show

lookupEach :: (Show k, Ord k, MonadState s m) => Getter s (Map k a) -> [k] -> m [a]
lookupEach getter keys = gets (view getter) >>= \sMap -> pure . reverse . foldl (foldFn sMap) [] $ keys
    where foldFn sMap acc key = (:acc) . fromMaybe (failWithKeyNotFound key) . Map.lookup key $ sMap

lookupSingle :: (Show k, Ord k, MonadState s m) => Getter s (Map k a) -> k -> m a
lookupSingle getter key = fromMaybe (failWithKeyNotFound key) . Map.lookup key <$> gets (view getter)

insertSL :: Ord k => Lens' l (Map k a) -> k -> a -> SystemLink l a
insertSL getter key val = modify (over getter $ Map.insert key val) $> val

-- Parsing for SystemLink l values

parseListSLWith :: (Value -> Parser (SystemLink l a)) -> Value -> Parser (SystemLink l [a])
parseListSLWith parser (Array a) = foldM append (pure []) a
    where append acc cur = parser cur >>= pure . (flip (liftM2 (:))) acc
parseListSLWith _ v              = typeMismatch "[SystemLink l a]" v

parseListSL :: FromJSON (SystemLink l a) => Value -> Parser (SystemLink l [a])
parseListSL = parseListSLWith parseJSON

parseMapSLWith :: Ord k => (Value -> Parser (SystemLink l k)) -> (Value -> Parser (SystemLink l a)) -> Value -> Parser (SystemLink l (Map k a))
parseMapSLWith keyParser parser (Object v) = (foldM append (pure []) . map toValue . Hash.toList $ v) >>>= pure . Map.fromList
    where append acc (key, val) = makeTuple key val >>= pure . (flip (liftM2 (:))) acc 
          makeTuple key val  = (liftM2 . liftM2) (,) (keyParser key) (parser val)
          toValue (k, v) = (String k, v)
parseMapSLWith _ _ v            = typeMismatch "Map k (SystemLink l a)" v

parseMapSL :: FromJSON (SystemLink l a) => Value -> Parser (SystemLink l (Map Text a))
parseMapSL = parseMapSLWith (fmap pure . parseJSON) parseJSON
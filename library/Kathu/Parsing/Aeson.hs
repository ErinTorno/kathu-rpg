{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We make use of orphaned instances in here to provide quick generation of JSON instances
-- making use of common types like V2, V3, V4, etc.

{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Parsing.Aeson where

import           Control.Monad         (foldM, liftM, liftM2)
import           Data.Aeson
import           Data.Aeson.Types      (Parser, typeMismatch)
import qualified Data.HashMap.Strict   as Hash
import           Data.Functor.Compose
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe, maybe)
import           Data.Text             (Text)
import qualified Data.Vector           as Vec
import           Foreign.C.Types       (CInt)
import           Linear.V2             (V2(..))
import           Linear.V3             (V3(..))
import           Linear.V4             (V4(..))

import           Kathu.Util.Collection (dropInitial)
import           Kathu.Util.Flow       ((>>>=))
import           Kathu.Util.Dependency

-- We drop the starting _ so that fields for lenses don't keep it
standardProjectOptions :: Options
standardProjectOptions = defaultOptions {fieldLabelModifier = camelTo2 '-' . dropInitial '_', omitNothingFields = True}

----------------------------
-- Parsing Util Functions --
----------------------------

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

parseListDPWith :: Monad m => (Value -> Parser (Dependency s m a)) -> Value -> Parser (Dependency s m [a])
parseListDPWith parser (Array a) = foldM append (pure []) a
    where append acc cur = parser cur >>= pure . (flip (liftM2 (:))) acc
parseListDPWith _ v              = typeMismatch "[Dependency s m a]" v

parseListDP :: (FromJSON (Dependency s m a), Monad m) => Value -> Parser (Dependency s m [a])
parseListDP = parseListDPWith parseJSON

parseMapDPWith :: (Monad m, Ord k) => (Value -> Parser (Dependency s m k)) -> (Value -> Parser (Dependency s m a)) -> Value -> Parser (Dependency s m (Map k a))
parseMapDPWith keyParser parser (Object v) = (foldM append (pure []) . map toValue . Hash.toList $ v) >>>= pure . Map.fromList
    where append acc (key, val) = makeTuple key val >>= pure . (flip (liftM2 (:))) acc 
          makeTuple key val  = (liftM2 . liftM2) (,) (keyParser key) (parser val)
          toValue (key, value) = (String key, value)
parseMapDPWith _ _ v            = typeMismatch "Map k (Dependency s m a)" v

parseMapDP :: (FromJSON (Dependency s m a), Monad m) => Value -> Parser (Dependency s m (Map Text a))
parseMapDP = parseMapDPWith (fmap pure . parseJSON) parseJSON

--------------------
-- Misc Instances --
--------------------

instance ToJSON CInt where
    toJSON i = toJSON $ (fromIntegral i :: Int)
instance FromJSON CInt where
    parseJSON a = fromIntegral <$> (parseJSON a :: Parser Int)

-- These instances are for types commonly used in this project from external libraries
-- but without ToJSON instances

-- Linear Vectors

-- Vectors can either be given as a object with named dimensions, or as an array
-- Ex: "myVec": {"x": 10.0, "y": -5.0}
--     "myVec": [10.0, -5.0]

-- V2
-- We serialize to an array now, as it avoids needing to use x y .. names as they might not always be appropriate for the specific vector

instance ToJSON a => ToJSON (V2 a) where
    -- toJSON (V2 x y) = object ["x" .= x, "y" .= y]
    toJSON (V2 x y) = toJSON [x, y]

instance FromJSON a => FromJSON (V2 a) where
    parseJSON (Object m) = V2 <$> m .: "x" <*> m .: "y"
    parseJSON (Array a)  = if Vec.length a /= 2 then fail "V2 array is not of length 2" else res
        where pInd = parseJSON . (Vec.!) a
              res  = pInd 0 >>= \x -> pInd 1 >>= \y -> pure $ V2 x y
    parseJSON e          = typeMismatch "V2" e

-- V3

instance ToJSON a => ToJSON (V3 a) where
    -- toJSON (V3 x y z) = object ["x" .= x, "y" .= y, "z" .= z]
    toJSON (V3 x y z) = toJSON [x, y, z]

instance FromJSON a => FromJSON (V3 a) where
    parseJSON (Object m) = V3 <$> m .: "x" <*> m .: "y" <*> m .: "z"
    parseJSON (Array a)  = if Vec.length a /= 3 then fail "V3 array is not of length 3" else res
        where pInd = parseJSON . (Vec.!) a
              res  = pInd 0 >>= \x -> pInd 1 >>= \y -> pInd 2 >>= \z -> pure $ V3 x y z
    parseJSON e          = typeMismatch "V3" e

-- V4

instance ToJSON a => ToJSON (V4 a) where
    -- toJSON (V4 t x y z) = object ["t" .= t, "x" .= x, "y" .= y, "z" .= z]
    toJSON (V4 t x y z) = toJSON [t, x, y, z]

instance FromJSON a => FromJSON (V4 a) where
    parseJSON (Object m) = V4 <$> m .: "t" <*> m .: "x" <*> m .: "y" <*> m .: "z"
    parseJSON (Array a)  = if Vec.length a /= 4 then fail "V4 array is not of length 4" else res
        where pInd = parseJSON . (Vec.!) a
              res  = pInd 0 >>= \t -> pInd 1 >>= \x -> pInd 2 >>= \y -> pInd 3 >>= \z -> pure $ V4 t x y z
    parseJSON e          = typeMismatch "V3" e

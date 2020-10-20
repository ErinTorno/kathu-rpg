{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We make use of orphaned instances in here to provide quick generation of JSON instances
-- making use of common types like V2, V3, V4, etc.

module Kathu.Parsing.Aeson where

import           Control.Monad         (foldM, liftM2)
import           Data.Aeson
import           Data.Aeson.Types      (Parser, typeMismatch)
import qualified Data.Bifunctor        as Bi
import qualified Data.HashMap.Strict   as Hash
import           Data.Functor.Compose
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Data.Vector           (Vector)
import qualified Data.Vector           as Vec
import           Foreign.C.Types       (CInt)
import           Linear.V2             (V2(..))
import           Linear.V3             (V3(..))
import           Linear.V4             (V4(..))

import           Kathu.Util.Containers (dropInitial)
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
(.:^) v = Compose . fmap return . (.:) v

-- gets a type encased in a monad from a Parser and Composes it
(.:~) :: (FromJSON (m a)) => Object -> Text -> Compose Parser m a
(.:~) v = Compose . (.:) v

-- same as .:^, but returns a Maybe
(.:^?) :: (Monad m, FromJSON a) => Object -> Text -> Compose Parser m (Maybe a)
(.:^?) v = Compose . fmap return . (.:?) v

-- same as .:~, but returns a Maybe
(.:~?) :: (Monad m, FromJSON (m a)) => Object -> Text -> Compose Parser m (Maybe a)
(.:~?) obj t = Compose (maybe (pure Nothing) (fmap Just) <$> obj .:? t)

(.!=~) :: Monad m => Compose Parser m (Maybe a) -> a -> Compose Parser m a
(.!=~) p def = fromMaybe def <$> p

parseListDPWith :: Monad m => (Value -> Parser (Dependency s m a)) -> Value -> Parser (Dependency s m [a])
parseListDPWith parser (Array a) = foldM append (pure []) a
    where append acc cur         = flip (liftM2 (:)) acc <$> parser cur
parseListDPWith _ v              = typeMismatch "[Dependency s m a]" v

parseMapDPWith :: (Monad m, Ord k) => (Value -> Parser (Dependency s m k)) -> (Value -> Parser (Dependency s m a)) -> Value -> Parser (Dependency s m (Map k a))
parseMapDPWith keyParser parser (Object v) = resultList v >>>= pure . Map.fromList
    where resultList            = foldM append (pure []) . map (Bi.first String) . Hash.toList
        
          append acc (key, val) = flip (liftM2 (:)) acc <$> makeTuple key val
          makeTuple key val     = (liftM2 . liftM2) (,) (keyParser key) (parser val)
parseMapDPWith _ _ v            = typeMismatch "Map k (Dependency s m a)" v

parseMapDP :: (FromJSON (Dependency s m a), FromJSON k, Ord k, Monad m) => Value -> Parser (Dependency s m (Map k a))
parseMapDP = parseMapDPWith (fmap pure . parseJSON) parseJSON

--------------------
-- Misc Instances --
--------------------

instance (FromJSON (Dependency s m a), Monad m) => FromJSON (Dependency s m [a]) where
    parseJSON = parseListDPWith parseJSON

instance (FromJSON (Dependency s m a), Monad m) => FromJSON (Dependency s m (Vector a)) where
    parseJSON v = getCompose $ Vec.fromList <$> Compose (parseListDPWith parseJSON v)

instance ToJSON CInt where
    toJSON i = toJSON (fromIntegral i :: Int)
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
    toJSON     (V2 x y) = toJSON     [x, y]
    toEncoding (V2 x y) = toEncoding [x, y]

instance FromJSON a => FromJSON (V2 a) where
    parseJSON (Object m)    = V2 <$> m .: "x" <*> m .: "y"
    parseJSON (Array a)
        | Vec.length a /= 2 = fail ("V2 array is not of length 2 (" ++ show a ++ ")")
        | otherwise         = V2 <$> pInd 0 <*> pInd 1
        where pInd          = parseJSON . (Vec.!) a

    parseJSON e             = typeMismatch "V2" e

-- V3

instance ToJSON a => ToJSON (V3 a) where
    toJSON (V3 x y z) = toJSON [x, y, z]

instance FromJSON a => FromJSON (V3 a) where
    parseJSON (Object m)    = V3 <$> m .: "x" <*> m .: "y" <*> m .: "z"
    parseJSON (Array a)
        | Vec.length a /= 3 = fail ("V3 array is not of length 3 (" ++ show a ++ ")")
        | otherwise         = V3 <$> pInd 0 <*> pInd 1 <*> pInd 2
        where pInd          = parseJSON . (Vec.!) a

    parseJSON e             = typeMismatch "V3" e

-- V4

instance ToJSON a => ToJSON (V4 a) where
    toJSON (V4 t x y z) = toJSON [t, x, y, z]

instance FromJSON a => FromJSON (V4 a) where
    parseJSON (Object m)    = V4 <$> m .: "t" <*> m .: "x" <*> m .: "y" <*> m .: "z"
    parseJSON (Array a)
        | Vec.length a /= 4 = fail ("V4 array is not of length 4 (" ++ show a ++ ")")
        | otherwise         = V4 <$> pInd 0 <*> pInd 1 <*> pInd 2 <*> pInd 3
        where pInd          = parseJSON . (Vec.!) a

    parseJSON e             = typeMismatch "V3" e

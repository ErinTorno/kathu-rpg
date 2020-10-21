{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We provide some instances for common typeclasses in here for other library types

module Verda.Util.Types
    ( Identifier(..)
    , IDMap
    , IDHashTable
    , Range(..)
    , TimeStamped(..)
    , mkIdentifier
    , emptyIDMap
    , textIDLens
    , clampBetween
    , clampRange
    ) where

import           Control.Lens
import           Control.Monad            (replicateM)
import           Control.Monad.ST         (RealWorld)
import           Data.Aeson
import qualified Data.Aeson.Encoding      as AE
import           Data.Aeson.Types         (typeMismatch)
import qualified Data.ByteString          as B
import           Data.Hashable
import           Data.HashTable.ST.Basic  (HashTable)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Serialize
import qualified Data.Serialize.Get       as SG
import qualified Data.Serialize.Put       as SP
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector              as Vec
import qualified Data.Vector.Generic      as GVec
import qualified Data.Vector.Storable     as SVec
import qualified Data.Vector.Unboxed      as UVec
import           Data.Word
import           GHC.Generics

----------------
-- Identifier --
----------------

-- | A type use to represent the type that identifies the objects that compose the game world
data Identifier = Identifier
    { hashID :: !Int
    , unID   :: !Text
    } deriving (Eq, Generic, Ord)

textIDLens :: Lens' Identifier Text
textIDLens = lens unID (const mkIdentifier)

mkIdentifier :: Text -> Identifier
mkIdentifier t = Identifier (hash t) t

instance Show Identifier where
    show (Identifier _ idt) = show idt

instance IsString Identifier where
    fromString = mkIdentifier . T.pack

instance ToJSONKey Identifier where
    toJSONKey = ToJSONKeyText unID (AE.text . unID)
instance FromJSONKey Identifier where
    fromJSONKey = FromJSONKeyText mkIdentifier

instance ToJSON Identifier where
    toJSON (Identifier _ identifier) = toJSON identifier
instance FromJSON Identifier where
    parseJSON (String s) = pure . mkIdentifier $ s
    parseJSON e          = typeMismatch "Identifier" e

instance Serialize Identifier where
    put (Identifier _ idt) = put idt
    get                    = mkIdentifier <$> get

instance Hashable Identifier where
    hash (Identifier h _) = h
    hashWithSalt i (Identifier _ idt) = hashWithSalt i idt

-- Frequent enough that it's nice to have an alias
type IDMap = Map Identifier

type IDHashTable a = HashTable RealWorld Identifier a

emptyIDMap :: IDMap a
emptyIDMap = Map.empty

-----------
-- Range --
-----------

-- | Represents a range of values between a lower bound and an upper bound
data Range a = Range
    { rangeMin :: !a
    , rangeMax :: !a
    } deriving (Show, Eq, Generic, Functor)

instance ToJSON a => ToJSON (Range a) where
    toJSON (Range rMin rMax) = toJSON [rMin, rMax]
instance FromJSON a => FromJSON (Range a) where
    parseJSON (Object m) = Range <$> m .: "min" <*> m .: "max"
    parseJSON (Array a)  = if Vec.length a /= 2 then fail "Range array is not of length 2" else res
        where pInd = parseJSON . (Vec.!) a
              res  = Range <$> pInd 0 <*> pInd 1
    parseJSON e          = typeMismatch "Range" e

instance Serialize a => Serialize (Range a)

clampBetween :: Ord a => a -> a -> a -> a
clampBetween rMin rMax cur
    | cur > rMax = rMax
    | cur < rMin = rMin
    | otherwise  = cur

clampRange :: Ord a => Range a -> a -> a
clampRange (Range rMin rMax) = clampBetween rMin rMax

-----------------
-- TimeStamped --
-----------------

data TimeStamped a = TimeStamped {timedVal :: !a, timeStamp :: !Word32} deriving (Eq)

instance Show a => Show (TimeStamped a) where
    show (TimeStamped v s) = concat ["{", show v, " at time ", show s, "ms}"]

instance Ord a => Ord (TimeStamped a) where
    (TimeStamped v1 t1) <= (TimeStamped v2 t2) = v1 < v2 || (v1 == v2 && t1 <= t2)

    (TimeStamped v1 t1) `compare` (TimeStamped v2 t2) = eval2 $ v1 `compare` v2
        where eval2 EQ  = t1 `compare` t2
              eval2 ord = ord

----------------------
-- Orphan Instances --
----------------------

instance Serialize Text where
    put txt = put (fromIntegral (B.length utf) :: Word32) >> SP.putByteString utf
        where utf = TE.encodeUtf8 txt
    get = TE.decodeUtf8With TE.lenientDecode <$> (SG.getByteString =<< get)

putVector :: (Serialize a, GVec.Vector v a) => v a -> Put
putVector v = put (fromIntegral (GVec.length v) :: Word32) >> GVec.mapM_ put v

getVector :: (Serialize a, GVec.Vector v a) => Get (v a)
getVector = GVec.fromList <$> (flip replicateM get =<< get)

instance Serialize a => Serialize (Vec.Vector a) where
    put = putVector
    get = getVector

instance (Serialize a, SVec.Storable a) => Serialize (SVec.Vector a) where
    put = putVector
    get = getVector

instance (Serialize a, UVec.Unbox a) => Serialize (UVec.Vector a) where
    put = putVector
    get = getVector
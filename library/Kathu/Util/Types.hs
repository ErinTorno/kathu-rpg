{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We provide some instances for common typeclasses in here for other library types

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Kathu.Util.Types
    ( Identifier(..)
    , IDMap
    , Range(..)
    ) where

import           Control.Monad            (replicateM)
import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import qualified Data.ByteString          as B
import           Data.Map                 (Map)
import           Data.Serialize
import qualified Data.Serialize.Get       as SG
import qualified Data.Serialize.Put       as SP
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TEErr
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
newtype Identifier = Identifier
    { unID :: Text -- If needed, later we can change this to be a int hash
    } deriving (Eq, Generic, Ord, IsString, FromJSONKey, Serialize)

instance Show Identifier where
    show (Identifier idt) = show idt
instance ToJSON Identifier where
    toJSON (Identifier identifier) = toJSON identifier
instance FromJSON Identifier where
    parseJSON (String s) = pure . Identifier $ s
    parseJSON e          = typeMismatch "Identifier" e

-- Frequent enough that it's nice to have an alias
type IDMap = Map Identifier

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

----------------------
-- Orphan Instances --
----------------------

instance Serialize Text where
    put txt = put (fromIntegral (B.length utf) :: Word32) >> SP.putByteString utf
        where utf = TE.encodeUtf8 txt
    get = TE.decodeUtf8With TEErr.lenientDecode <$> (SG.getByteString =<< get)

putVector :: (Serialize a, GVec.Vector v a) => v a -> Put
putVector v = put (fromIntegral (GVec.length v) :: Word32) >> GVec.mapM_ put v

getVector :: (Serialize a, GVec.Vector v a) => Get (v a)
getVector = GVec.fromList <$> ((flip replicateM) get =<< get)

instance Serialize a => Serialize (Vec.Vector a) where
    put = putVector
    get = getVector

instance (Serialize a, SVec.Storable a) => Serialize (SVec.Vector a) where
    put = putVector
    get = getVector

instance (Serialize a, UVec.Unbox a) => Serialize (UVec.Vector a) where
    put = putVector
    get = getVector
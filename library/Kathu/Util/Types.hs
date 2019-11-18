{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric,  DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Util.Types where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Map (Map)
import Data.String
import Data.Text (Text)
import qualified Data.Vector as Vec
import GHC.Generics

----------------
-- Identifier --
----------------

-- | A type use to represent the type that identifies the objects that compose the game world
newtype Identifier = Identifier
    { unID :: Text
    } deriving (Eq, Generic, Ord, IsString, FromJSONKey)

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
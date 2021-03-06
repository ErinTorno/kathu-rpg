{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.Resource where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Serialize
import Control.Lens     hiding ((.=))
import GHC.Generics

import Kathu.Util.Types (clampBetween)

-- | A resource that is not expected to change frequently
data Static a = Static {_stcBase :: !a, _stcBonus :: !a} deriving (Show, Eq, Functor, Generic)
makeLenses ''Static

instance Serialize a => Serialize (Static a)

-- the resources can be loaded from either a single number (in which case the rest will be assumed), or from an object
instance ToJSON a => ToJSON (Static a) where
    toJSON (Static base bonus) = object ["base" .= base, "bonus" .= bonus]

    toEncoding (Static base bonus) = pairs ("base" .= base <> "bonus" .= bonus)

instance (FromJSON a, Num a) => FromJSON (Static a) where
    parseJSON (Object m)   = Static <$> m .: "base" <*> m .: "bonus"
    parseJSON n@(Number _) = flip Static 0 <$> parseJSON n
    parseJSON e            = typeMismatch "Static" e

-- | A resource that has a current value that ranges between the maximm and zero
data Dynamic a = Dynamic {_dynCur :: !a, _dynMax :: !a, _dynBonus :: !a} deriving (Show, Eq, Functor, Generic)
makeLenses ''Dynamic

instance Serialize a => Serialize (Dynamic a)

instance ToJSON a => ToJSON (Dynamic a) where
    toJSON (Dynamic cur base bonus) = object ["cur" .= cur, "base" .= base, "bonus" .= bonus]

    toEncoding (Dynamic cur base bonus) = pairs ("cur" .= cur <> "base" .= base <> "bonus" .= bonus)

instance (FromJSON a, Num a) => FromJSON (Dynamic a) where
    parseJSON (Object m)   = Dynamic <$> m .: "cur" <*> m .: "base" <*> m .: "bonus"
    parseJSON n@(Number _) = (\base -> Dynamic base base 0) <$> parseJSON n
    parseJSON e            = typeMismatch "Dynamic" e

-- Functions for working with these

maxDynTotal :: Num a => Dynamic a -> a
maxDynTotal (Dynamic _ dynMaximum bonus) = dynMaximum + bonus

modDynCur :: (Num a, Ord a) => a -> Dynamic a -> Dynamic a
modDynCur dval dyn = over dynCur (clampBetween 0 (maxDynTotal dyn) . (+dval)) dyn

modDynBonus :: Num a => a -> Dynamic a -> Dynamic a
modDynBonus dval = over dynBonus (+dval)

modStcBonus :: Num a => a -> Static a -> Static a
modStcBonus dval = over stcBonus (+dval)

total :: Num a => Static a -> a
total (Static base bonus) = base + bonus

totalMaximum :: Num a => Dynamic a -> a
totalMaximum (Dynamic _ dynMaximum bonus) = dynMaximum + bonus
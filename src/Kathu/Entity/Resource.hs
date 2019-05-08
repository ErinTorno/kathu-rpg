{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.Resource where

import Control.Lens
import GHC.Generics
import Kathu.Util.Misc

-- | A resource that is not expected to change frequently
data Static a = Static {_stcBase :: a, _stcBonus :: a} deriving (Show, Eq, Functor, Generic)
makeLenses ''Static

-- | A resource that has a current value that ranges between the maximm and zero
data Dynamic a = Dynamic {_dynCur :: a, _dynMax :: a, _dynBonus :: a} deriving (Show, Eq, Functor, Generic)
makeLenses ''Dynamic

-- Functions for working with these

maxDynTotal :: (Num a, Ord a) => Dynamic a -> a
maxDynTotal (Dynamic _ max bon) = max + bon

modDynCur :: (Num a, Ord a) => a -> Dynamic a -> Dynamic a
modDynCur dval dyn = over dynCur (clampBetween 0 (maxDynTotal dyn) . (+dval)) dyn

modDynBonus :: (Num a, Ord a) => a -> Dynamic a -> Dynamic a
modDynBonus dval = over dynBonus (+dval)

modStcBonus :: (Num a, Ord a) => a -> Static a -> Static a
modStcBonus dval = over stcBonus (+dval)

total :: Num a => Static a -> a
total (Static base bon) = base + bon

totalMaximum :: Num a => Dynamic a -> a
totalMaximum (Dynamic _ max bon) = max + bon
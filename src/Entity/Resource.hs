{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Entity.Resource where

import GHC.Generics

data Static a = Static {stcBase :: a, stcBonus :: a} deriving (Show, Eq, Functor, Generic)

data Dynamic a = Dynamic {dynCur :: a, dynMax :: a, dynBonus :: a} deriving (Show, Eq, Functor, Generic)

total :: Num a => Static a -> a
total (Static base bon) = base + bon

totalMaximum :: Num a => Dynamic a -> a
totalMaximum (Dynamic _ max bon) = max + bon

modifyCurrent :: Ord a => Num a => a -> Dynamic a -> Dynamic a
modifyCurrent dv (Dynamic cur max bonus)
    | cur + dv > max = Dynamic max max bonus
    | cur + dv < 0   = Dynamic 0 max bonus

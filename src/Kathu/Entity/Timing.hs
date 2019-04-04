{-# LANGUAGE BangPatterns #-}

module Kathu.Entity.Timing where

import Data.Word

data TimeStamped a = TimeStamped {timedVal :: !a, timeStamp :: Word32} deriving (Eq)

instance Show a => Show (TimeStamped a) where
    show (TimeStamped v s) = concat ["{", show v, " at time ", show s, "ms}"]

instance Ord a => Ord (TimeStamped a) where
    (TimeStamped v1 t1) <= (TimeStamped v2 t2) = v1 < v2 || (v1 == v2 && t1 <= t2)

    (TimeStamped v1 t1) `compare` (TimeStamped v2 t2) = eval2 $ v1 `compare` v2
        where eval2 EQ  = t1 `compare` t2
              eval2 ord = ord
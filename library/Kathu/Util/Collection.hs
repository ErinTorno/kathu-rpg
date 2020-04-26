{-# LANGUAGE BangPatterns #-}

module Kathu.Util.Collection where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MVec
import           Numeric                     (showHex)

-- Maybe functions

fromJustElseError :: String -> Maybe a -> a
fromJustElseError _ (Just a) = a
fromJustElseError errMsg _   = error errMsg

-- List functions

-- | Drops the first element if it equals the given value
dropInitial :: Eq a => a -> [a] -> [a]
dropInitial _ [] = []
dropInitial p st@(x:xs) | p == x    = xs
                        | otherwise = st

-- | Breaks a list into all item before a given element, and all items after that element
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst _ [] = ([], [])
splitAtFirst e l  = go [] l
    where go acc []                 = (acc, [])
          go acc (x:xs) | x == e    = (acc, xs)
                        | otherwise = go (x:acc) xs

-- Map functions

findByElem :: (a -> Bool) -> Map k a -> Maybe (k, a)
findByElem cond = Map.foldlWithKey' isNext Nothing
    where isNext Nothing k a  = if cond a then Just (k, a) else Nothing
          isNext (Just p) _ _ = Just p

-- Vector functions

iterMVec :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> m b) -> m ()
iterMVec !vec !f = go 0
    where go !i | i == MVec.length vec = pure ()
                | otherwise            = MVec.unsafeRead vec i >>= f >> go (i + 1)

forMVec :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> a) -> m ()
forMVec !vec !f = go 0
    where go !i | i == MVec.length vec = pure ()
                | otherwise            = MVec.unsafeModify vec f i >> go (i + 1)

forMMVec :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (a -> m a) -> m ()
forMMVec !vec !f = go 0
    where go !i | i == MVec.length vec = pure ()
                | otherwise            = MVec.unsafeRead vec i >>= f >>= MVec.unsafeWrite vec i >> go (i + 1)

-- Specialized List functions

padShowHex :: (Integral a, Show a) => Int -> a -> ShowS
padShowHex l a = (++) . pad . showHex a $ ""
    where pad s = let len = length s in if len <= l then replicate (l - len) '0' ++ s else s
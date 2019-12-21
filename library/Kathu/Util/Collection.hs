{-# LANGUAGE BangPatterns #-}

module Kathu.Util.Collection where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Char (toLower, toUpper)
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVec
import           Numeric (showHex)

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

-- Vector functions

iterMVec :: PrimMonad m => MVector (PrimState m) a -> (a -> m b) -> m ()
iterMVec !vec !f = go 0
    where go !i | i == MVec.length vec = pure ()
                | otherwise            = MVec.unsafeRead vec i >>= f >> go (i + 1)

mapMVec :: PrimMonad m => MVector (PrimState m) a -> (a -> a) -> m ()
mapMVec !vec !f = go 0
    where go !i | i == MVec.length vec = pure ()
                | otherwise            = MVec.unsafeModify vec f i >> go (i + 1)

mapMMVec :: PrimMonad m => MVector (PrimState m) a -> (a -> m a) -> m ()
mapMMVec !vec !f = go 0
    where go !i | i == MVec.length vec = pure ()
                | otherwise            = MVec.unsafeRead vec i >>= f >>= MVec.unsafeWrite vec i >> go (i + 1)

growMVecIfNeeded :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m (MVector (PrimState m) a)
growMVecIfNeeded !vec !sizeIncrease !i
    | i < MVec.length vec = pure vec
    | otherwise           = MVec.unsafeGrow vec sizeIncrease

-- Specialized List functions

padShowHex :: (Integral a, Show a) => Int -> a -> ShowS
padShowHex l a = (++) . pad . showHex a $ ""
    where pad s = let len = length s in if len <= l then replicate (l - len) '0' ++ s else s

toPascalCase :: String -> String
toPascalCase []     = []
toPascalCase (c:cs) = toUpper c: cs

toCamelCase :: String -> String
toCamelCase []     = []
toCamelCase (c:cs) = toLower c: cs
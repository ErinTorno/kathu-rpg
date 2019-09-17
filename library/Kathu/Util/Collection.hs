{-# LANGUAGE BangPatterns #-}

module Kathu.Util.Collection where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Char (toLower, toUpper)
import qualified Data.Vector.Mutable as MVec
import Numeric (showHex)

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

growMVecIfNeeded :: PrimMonad m => Int -> Int -> MVec.MVector (PrimState m) a -> m (MVec.MVector (PrimState m) a)
growMVecIfNeeded !sizeIncrease !i !vec
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
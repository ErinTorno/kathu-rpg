{-# LANGUAGE DeriveFunctor #-}

module Kathu.Util.Misc where

import Data.Bool
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Mutable as MVec
import Numeric (showHex)

-- Util types

data Range a = Range {rangeMin :: a, rangeMax :: a} deriving (Show, Eq, Functor)

clampBetween :: Ord a => a -> a -> a -> a
clampBetween min max cur | cur > max = max
                         | cur < min = min
                         | otherwise = cur

clampRange :: Ord a => Range a -> a -> a
clampRange (Range min max) = clampBetween min max

closestToZero :: (Num a, Ord a) => a -> a -> a
closestToZero x y = if x' > y' then y else x
    where (x', y') = (abs x, abs y)

farthestFromZero :: (Num a, Ord a) => a -> a -> a
farthestFromZero x y = if x' > y' then x else y
    where (x', y') = (abs x, abs y)

-- Tuple functions

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (v1, v2) = (f v1, f v2)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- Monad functions

(>>>=) :: (Monad m, Monad n) => m (n a) -> (a -> n b) -> m (n b)
(>>>=) v f = v >>= \v' -> return (v' >>= f)
infixl 1 >>>=

whileM :: Monad m => m Bool -> m ()
whileM b = b >>= bool (return ()) (whileM b)

whileFstM :: Monad m => m (Bool, a) -> m a
whileFstM f = f >>= \(b, c) -> bool (return c) (whileFstM f) b

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []      = pure ([], [])
partitionM fn (x:xs) = fn x >>= app
    where next       = partitionM fn xs
          app True   = (\(u, v) -> (x:u, v)) <$> next
          app False  = (\(u, v) -> (u, x:v)) <$> next

-- Vector functions

growMVecIfNeeded s i vec
    | i < MVec.length vec = pure vec
    | otherwise           = MVec.unsafeGrow vec s

-- String/Text functions

padShowHex :: (Integral a, Show a) => Int -> a -> ShowS
padShowHex l a = (++) . pad . showHex a $ ""
    where pad s = let len = length s in if len <= l then replicate (l - len) '0' ++ s else s

showText :: Show a => a -> Text
showText = T.pack . show

dropInitial :: Char -> String -> String
dropInitial _ [] = []
dropInitial p st@(c:cs) | p == c = cs
                        | otherwise = st

toPascalCase :: String -> String
toPascalCase []     = []
toPascalCase (c:cs) = toUpper c: cs

toCamelCase :: String -> String
toCamelCase []     = []
toCamelCase (c:cs) = toLower c: cs
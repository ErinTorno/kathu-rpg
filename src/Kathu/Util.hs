{-# LANGUAGE DeriveFunctor #-}

module Kathu.Util where

import Data.Bool
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified SDL

-- Util types

data Range a = Range {rangeMin :: a, rangeMax :: a} deriving (Show, Eq, Functor)

clampBetween :: Ord a => a -> a -> a -> a
clampBetween min max cur | cur > max = max
                         | cur < min = min
                         | otherwise = cur

clampRange :: Ord a => Range a -> a -> a
clampRange (Range min max) = clampBetween min max

-- Tuple functions

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (v1, v2) = (f v1, f v2)

-- Monad functions

whileM :: Monad m => m Bool -> m ()
whileM b = b >>= bool (return ()) (whileM b)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []      = pure ([], [])
partitionM fn (x:xs) = fn x >>= app
    where next       = partitionM fn xs
          app True   = (\(u, v) -> (x:u, v)) <$> next
          app False  = (\(u, v) -> (u, x:v)) <$> next

-- Text functions

showText :: Show a => a -> Text
showText = T.pack . show

toPascalCase :: String -> String
toPascalCase []     = []
toPascalCase (c:cs) = toUpper c: cs

toCamelCase :: String -> String
toCamelCase []     = []
toCamelCase (c:cs) = toLower c: cs

toKebabCase :: String -> String
toKebabCase []     = []
toKebabCase (c:cs) = toLower c : apply cs
    where apply []     = []
          apply (c:cs) | isLower c = c : apply cs
                       | otherwise = toLower c : '-' : apply cs
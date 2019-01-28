{-# LANGUAGE DeriveFunctor #-}

module Util where

import Data.Bool
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Foreign.C.Types as CTypes
import qualified SDL

-- so if we even change in the future, it'll be an easier switch
type Vector2 = SDL.V2
type Vector3 = SDL.V3

type CInt = CTypes.CInt

-- Util types

data Range a = Range {rangeMin :: a, rangeMax :: a} deriving (Show, Eq, Functor)

restrictRange :: Ord a => Range a -> a -> a
restrictRange rng cur | cur < rangeMin rng = rangeMin rng
                      | cur > rangeMax rng = rangeMax rng
                      | otherwise          = cur

-- Monad Functions

whileM :: Monad m => m Bool -> m ()
whileM b = b >>= bool (return ()) (whileM b)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []      = pure ([], [])
partitionM fn (x:xs) = fn x >>= app
    where next       = partitionM fn xs
          app True   = (\(u, v) -> (x:u, v)) <$> next
          app False  = (\(u, v) -> (u, x:v)) <$> next

-- Text Functions

showText :: Show a => a -> Text
showText = T.pack . show

toPascalCase :: String -> String
toPascalCase [] = []
toPascalCase (c:cs) = toUpper c: cs

toCamelCase :: String -> String
toCamelCase [] = []
toCamelCase (c:cs) = toLower c: cs
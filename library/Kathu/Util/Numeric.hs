module Kathu.Util.Numeric where

import Data.Scientific

import Kathu.Util.Types (Range(..))

closestToZero :: (Num a, Ord a) => a -> a -> a
closestToZero x y = if x' > y' then y else x
    where (x', y') = (abs x, abs y)
 
farthestFromZero :: (Num a, Ord a) => a -> a -> a
farthestFromZero x y = if x' > y' then x else y
    where (x', y') = (abs x, abs y)

clampBetween :: Ord a => a -> a -> a -> a
clampBetween rMin rMax cur
    | cur > rMax = rMax
    | cur < rMin = rMin
    | otherwise      = cur

clampRange :: Ord a => Range a -> a -> a
clampRange (Range rMin rMax) = clampBetween rMin rMax

fromScientific :: Fractional a => Scientific -> a
fromScientific = fromRational . toRational
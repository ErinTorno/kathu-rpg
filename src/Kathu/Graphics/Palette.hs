{-# LANGUAGE DeriveGeneric #-}

module Kathu.Graphics.Palette where

import Data.Map (Map)
import qualified Data.Map as Map
import Kathu.Graphics.Color

data Layer = Outline | Background deriving (Show, Eq)

newtype Filter = Filter (Map Layer (Color -> Color))

filterFunction :: Layer -> Filter -> (Color -> Color)
filterFunction l (Filter m) = Map.findWithDefault id l m

-- Layer instances

instance Ord Layer where
    compare a b = fromEnum a `compare` fromEnum b
    (<=) a b    = fromEnum a <= fromEnum b

instance Enum Layer where
    fromEnum Outline    = 0
    fromEnum Background = 1

    toEnum 0 = Outline
    toEnum 1 = Background
    toEnum i = error . concat $ ["Unable to convert ", show i, " to layer"]
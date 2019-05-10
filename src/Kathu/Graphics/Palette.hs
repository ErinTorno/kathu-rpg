{-# LANGUAGE TupleSections #-}

module Kathu.Graphics.Palette where

import Data.Foldable
import Kathu.Graphics.Color
    
data Palette = Palette
    { background :: Color
    , filter :: Filter
    }

newtype Filter = Filter {unFilter :: Color -> Color}

composeFilters :: Foldable f => f Filter -> Filter
composeFilters = Filter . foldl (\acc -> (.acc) . unFilter) id
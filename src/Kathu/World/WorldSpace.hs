{-# LANGUAGE OverloadedStrings #-}

module Kathu.World.WorldSpace where

import Control.Lens
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Kathu.Graphics.Color (black)
import Kathu.Graphics.Palette

data WorldSpace = WorldSpace
    { worldID :: Text
    , worldPalettes :: Vector Palette
    -- Some way to hold the fields that it possesses
    }

emptyWorldSpace :: WorldSpace
emptyWorldSpace = WorldSpace "" $ Vec.singleton (Palette black id)
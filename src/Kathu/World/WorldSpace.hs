{-# LANGUAGE OverloadedStrings #-}

module Kathu.World.WorldSpace where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Kathu.Graphics.Color (black)
import Kathu.Graphics.Palette
import Kathu.World.Field

data WorldSpace = WorldSpace
    { worldID :: Text
    , worldPalettes :: Vector Palette
    , worldFields :: FieldSet
    -- Some way to hold the fields that it possesses
    }

emptyWorldSpace :: WorldSpace
emptyWorldSpace = WorldSpace "" (Vec.singleton (Palette black id)) Map.empty
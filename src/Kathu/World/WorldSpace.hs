{-# LANGUAGE OverloadedStrings #-}

module Kathu.World.WorldSpace where

import Control.Lens
import Data.Text (Text)
import Kathu.Graphics.Color (Color, mkColor)

data WorldSpace = WorldSpace
    { worldID :: Text
    , worldBgColor :: Color
    -- Some way to hold the fields that it possesses
    }

emptyWorldSpace :: WorldSpace
emptyWorldSpace = WorldSpace "" (mkColor 0 0 0 0)
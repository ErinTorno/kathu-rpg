module Kathu.Graphics.Palette where

import Kathu.Graphics.Color
    
data Palette = Palette
    { background :: Color
    , filter :: Color -> Color
    }
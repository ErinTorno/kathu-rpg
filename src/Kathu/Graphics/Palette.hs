{-# LANGUAGE TupleSections #-}

module Kathu.Graphics.Palette where

import Data.Foldable
import Kathu.Graphics.Color
    
data Palette = Palette
    { background :: Color
    , shader :: Maybe Shader
    }

newtype Shader = Shader {unShader :: Color -> Color}

composeShaders :: Foldable f => f Shader -> Shader
composeShaders = Shader . foldl (\acc -> (.acc) . unShader) id
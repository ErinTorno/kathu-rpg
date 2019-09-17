{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Graphics.Palette where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Kathu.Graphics.Color
    
data Palette = Palette
    { background :: Color
    , shader :: Maybe Shader
    }

instance FromJSON Palette where
    parseJSON (Object v) = Palette <$> v .: "background" <*> v .:? "shader"
    parseJSON e          = typeMismatch "Palette" e

newtype Shader = Shader {unShader :: Color -> Color}

instance FromJSON Shader where
    parseJSON (Array a)  = composeShaders <$> (mapM parseJSON a)
    parseJSON (Object v) = v .: "fn" >>= parseFn
        where parseFn str = fmap Shader $ case str of
                  "desaturate"    -> pure desaturate
                  "desaturate-by" -> desaturateBy <$> v .: "percent"
                  "blend-color"   -> blendColor <$> v .: "percent" <*> v .: "color"
                  "shift-hue"     -> (fromHSVFunction . shiftHue) <$> v .: "angle"
                  "invert-hue"    -> pure . fromHSVFunction $ invertHue
                  "invert-rgb"    -> pure invertRGB
                  "shift-hue-towards"     -> fromHSVFunction <$> (shiftHueTowards <$> v .: "angle" <*> v .: "percent")
                  "shift-hue-towards-abs" -> fromHSVFunction <$> (shiftHueTowardsAbs <$> v .: "target-angle" <*> v .: "shift-angle")
                  f               -> error $ "Attempted to parse unknown filter " ++ f
    parseJSON v = typeMismatch "Shader" v

composeShaders :: Foldable f => f Shader -> Shader
composeShaders = Shader . foldl (\acc -> (.acc) . unShader) id
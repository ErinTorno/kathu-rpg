{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- this module supports the encoding of SDL types

module IO.SDL where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as Vec
import Data.Text (Text)
import Foreign.C.Types (CInt)
import GHC.Generics
import qualified SDL

-- Keys

instance ToJSON SDL.Scancode
instance FromJSON SDL.Scancode

instance ToJSON SDL.Keycode
instance FromJSON SDL.Keycode

-- Vectors

-- Vectors can either be given as a object with named dimensions, or as an array
-- Ex: "myVec": {"x": 10.0, "y": -5.0}
--     "myVec": [10.0, -5.0]

-- V2
-- We serialize to an array now, as it avoids needing to use x y .. names as they might not always be appropriate for the specific vector

instance ToJSON a => ToJSON (SDL.V2 a) where
    -- toJSON (SDL.V2 x y) = object ["x" .= x, "y" .= y]
    toJSON (SDL.V2 x y) = toJSON [x, y]

instance FromJSON a => FromJSON (SDL.V2 a) where
    parseJSON (Object m) = SDL.V2 <$> m .: "x" <*> m .: "y"
    parseJSON (Array a)  = if Vec.length a /= 2 then fail "V2 array is not of length 2" else res
        where pInd = parseJSON . (Vec.!) a
              res  = pInd 0 >>= \x -> pInd 1 >>= \y -> pure $ SDL.V2 x y
    parseJSON e          = typeMismatch "V2" e

-- V3

instance ToJSON a => ToJSON (SDL.V3 a) where
    -- toJSON (SDL.V3 x y z) = object ["x" .= x, "y" .= y, "z" .= z]
    toJSON (SDL.V3 x y z) = toJSON [x, y, z]

instance FromJSON a => FromJSON (SDL.V3 a) where
    parseJSON (Object m) = SDL.V3 <$> m .: "x" <*> m .: "y" <*> m .: "z"
    parseJSON (Array a)  = if Vec.length a /= 3 then fail "V3 array is not of length 3" else res
        where pInd = parseJSON . (Vec.!) a
              res  = pInd 0 >>= \x -> pInd 1 >>= \y -> pInd 2 >>= \z -> pure $ SDL.V3 x y z
    parseJSON e          = typeMismatch "V3" e

-- V4

instance ToJSON a => ToJSON (SDL.V4 a) where
    -- toJSON (SDL.V4 t x y z) = object ["t" .= t, "x" .= x, "y" .= y, "z" .= z]
    toJSON (SDL.V4 t x y z) = toJSON [t, x, y, z]

instance FromJSON a => FromJSON (SDL.V4 a) where
    parseJSON (Object m) = SDL.V4 <$> m .: "t" <*> m .: "x" <*> m .: "y" <*> m .: "z"
    parseJSON (Array a)  = if Vec.length a /= 4 then fail "V4 array is not of length 4" else res
        where pInd = parseJSON . (Vec.!) a
              res  = pInd 0 >>= \t -> pInd 1 >>= \x -> pInd 2 >>= \y -> pInd 3 >>= \z -> pure $ SDL.V4 t x y z
    parseJSON e          = typeMismatch "V3" e

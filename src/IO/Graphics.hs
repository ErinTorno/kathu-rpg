{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Graphics where

import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Functor
import Data.Functor.Compose
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import GHC.Generics
import qualified SDL
import qualified SDL.Image as SDLI

import Graphics.Color
import Graphics.Drawable
import IO.Parsing
import IO.SDL

-- most of these use SystemLink as they require loading images with them or other SystemLink required types

zeroPoint :: SDL.Point SDL.V2 CInt
zeroPoint = SDL.P $ SDL.V2 0 0

insertImage :: Text -> SDL.Surface -> SystemLink SDL.Surface
insertImage key img = modify insert $> img
    where insert p = p {images = Map.insert key img . images $ p}

-- loads an image if it hasn't already been loaded and add it to our map, otherwise get that
loadImage :: Text -> SystemLink SDL.Surface
loadImage t = do
    url    <- T.pack <$> (parseUrl . T.unpack $ t)
    images <- gets images
    case Map.lookup url images of
        Just img -> liftSL . pure $ img
        Nothing  -> (liftSL . SDLI.load . T.unpack $ url) >>= insertImage url

getSurfaceBounds :: MonadIO m => SDL.Surface -> m (SDL.Rectangle CInt)
getSurfaceBounds s = SDL.surfaceDimensions s >>= return . SDL.Rectangle zeroPoint

instance FromJSON AnimationStrip where
    parseJSON = withObject "AnimationStrip" $ \v -> AnimationStrip <$> v .: "id" <*> v .: "frames" <*> v .: "row" <*> v .: "delay"

instance FromJSON (SystemLink Animation) where
    parseJSON (Object v) = do
        atlas  <- loadImage <$> v .: "atlas"
        strips <- v .: "strips"
        bounds <- (fmap fromIntegral <$> (v .: "bounds" :: Parser (SDL.V2 Int))) :: Parser (SDL.V2 CInt)
        pure (atlas >>= \a -> pure $ Animation a strips bounds)
    parseJSON v = typeMismatch "Animation" v

instance FromJSON (SystemLink RenderSprite) where
    parseJSON (String s)     = pure $ loadImage s >>= \i -> liftSL (getSurfaceBounds i) >>= \bnds -> pure $ StaticSprite i bnds
    parseJSON obj@(Object _) = (parseJSON obj :: Parser (SystemLink Animation)) >>= \img -> pure $ img >>= \a -> pure $ AnimatedSprite a 0 0 0
    parseJSON v              = typeMismatch "RenderSprite" v
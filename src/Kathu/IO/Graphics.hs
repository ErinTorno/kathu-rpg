{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.IO.Graphics where

import Control.Lens
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
import Kathu.Graphics.Color
import Kathu.Graphics.Drawable
import Kathu.IO.Misc
import Kathu.IO.Parsing
import qualified SDL
import qualified SDL.Image as SDLI

-- most of these use SystemLink as they require loading images with them or other SystemLink required types

zeroPoint :: SDL.Point SDL.V2 CInt
zeroPoint = SDL.P $ SDL.V2 0 0

insertImage :: Text -> Image -> SystemLink Image
insertImage key img = modify (over images $ Map.insert key img) $> img

-- loads an image if it hasn't already been loaded and add it to our map, otherwise get that
loadImage :: Text -> SystemLink Image
loadImage t = do
    url    <- T.pack <$> (parseUrl . T.unpack $ t)
    images <- gets (view images)
    case Map.lookup url images of
        Just img -> liftSL . pure $ img
        Nothing  -> (liftSL . SDLI.load . T.unpack $ url) >>= insertImage url

getSurfaceBounds :: MonadIO m => SDL.Surface -> m (SDL.Rectangle CInt)
getSurfaceBounds s = SDL.surfaceDimensions s >>= pure . SDL.Rectangle zeroPoint

getTextureBounds :: MonadIO m => SDL.Texture -> m (SDL.Rectangle CInt)
getTextureBounds t = SDL.queryTexture t >>= \t -> pure . SDL.Rectangle zeroPoint $ SDL.V2 (SDL.textureWidth t) (SDL.textureHeight t)

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
    parseJSON (String s)     = pure $ loadImage s >>= \i -> liftSL (getSurfaceBounds i) >>= \bnds -> pure . RSStatic $ StaticSprite i bnds
    parseJSON obj@(Object _) = (parseJSON obj :: Parser (SystemLink Animation)) >>= \img -> pure $ img >>= \a -> pure . RSAnimated $ AnimatedSprite a 0 0 0
    parseJSON v              = typeMismatch "RenderSprite" v

-- Color

instance ToJSON Color where
    toJSON = toJSON . show
instance FromJSON Color where
    parseJSON (String s) = pure . read . T.unpack $ s
    parseJSON e = typeMismatch "SpecialEntity" e
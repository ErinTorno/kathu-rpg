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
import qualified Data.Vector as Vec
import Foreign.C.Types (CInt)
import GHC.Generics
import Kathu.Graphics.Color
import Kathu.Graphics.Drawable
import Kathu.Graphics.Palette
import Kathu.IO.Misc
import Kathu.IO.Parsing
import Kathu.Util ((>>>=))
import qualified SDL
import qualified SDL.Image as SDLI

-- most of these use SystemLink as they require loading images with them or other SystemLink required types

zeroPoint :: SDL.Point SDL.V2 CInt
zeroPoint = SDL.P $ SDL.V2 0 0

instance FromJSON (SystemLink ImageID) where
    parseJSON (String s) = pure $ (ImageID . fromIntegral) <$> ((flip $ lookupOrAddSL category) adder =<< url)
        where category = "ImageID"
              url = fmap T.pack . parseUrl . T.unpack $ s
              adder = do
                  url' <- url
                  modify $ over countingIDs (Map.adjust (mapInsertIncr url') category)
                  image <- liftSL . SDLI.load . T.unpack $ url'
                  modify $ over plImages ((flip Vec.snoc) image)
    parseJSON v          = typeMismatch "ImageID" v

getSurfaceBounds :: ImageID -> SystemLink (SDL.Rectangle CInt)
getSurfaceBounds (ImageID iid) = SDL.Rectangle zeroPoint <$> surfaceDim
    where surfaceDim :: SystemLink (SDL.V2 CInt)
          surfaceDim = image >>= liftSL . SDL.surfaceDimensions
          image :: SystemLink Image
          image = (Vec.!iid) <$> gets (view plImages)

getTextureBounds :: MonadIO m => SDL.Texture -> m (SDL.Rectangle CInt)
getTextureBounds t = SDL.queryTexture t >>= \t -> pure . SDL.Rectangle zeroPoint $ SDL.V2 (SDL.textureWidth t) (SDL.textureHeight t)

instance FromJSON AnimationStrip where
    parseJSON = withObject "AnimationStrip" $ \v -> AnimationStrip <$> v .: "id" <*> v .: "frames" <*> v .: "row" <*> v .: "delay"

instance FromJSON (SystemLink Animation) where
    parseJSON (Object v) = getCompose $ Animation
        <$> v .:~ "atlas"
        <*> v .:^ "strips"
        <*> v .:^ "bounds"
    parseJSON v = typeMismatch "Animation" v

instance FromJSON (SystemLink RenderSprite) where
    parseJSON s@(String _) = parseJSON s >>>= \iid -> getSurfaceBounds iid >>= \bnd -> pure . RSStatic . (flip StaticSprite) bnd $ iid
    parseJSON o@(Object _) = parseJSON o >>>= \iid -> pure . RSAnimated $ AnimatedSprite iid 0 0 0 
    parseJSON v            = typeMismatch "RenderSprite" v

-- Color

instance ToJSON Color where
    toJSON = toJSON . show
instance FromJSON Color where
    parseJSON (String s) = pure . read . T.unpack $ s
    parseJSON e = typeMismatch "SpecialEntity" e

instance FromJSON Palette where
    parseJSON = withObject "Palette" $ \v -> Palette <$> v .: "background" <*> pure id
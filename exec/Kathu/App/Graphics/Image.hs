{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MonoLocalBinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.App.Graphics.Image where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified SDL
import qualified SDL.Image as SDLI

import Kathu.IO.Directory
import Kathu.Parsing.Counting
import Kathu.Util.Dependency

type Image = SDL.Surface

newtype ImageID = ImageID Int deriving (Show, Eq, Ord)

instance (s `CanProvide` WorkingDirectory, MonadIO m) => FromJSON (Dependency s m Image) where
    parseJSON (String s) = pure $ (resolveAssetPathDP . T.unpack) s
                       >>= liftDependency . SDLI.load
    parseJSON v          = typeMismatch "Image" v

instance ( s `CanProvide` WorkingDirectory
         , s `CanStoreEach`  '[CountingIDs, (Vector SDL.Surface)]
         , MonadIO m
         ) => FromJSON (Dependency s m ImageID) where
    parseJSON (String s) = pure $ (ImageID . fromIntegral) <$> (url >>= lookupOrExecAndVerify adder "ImageID")
        where url   = fmap T.pack . resolveAssetPathDP . T.unpack $ s
              adder = do url'       <- url
                         image      <- liftDependency . SDLI.load . T.unpack $ url'
                         images     <- readStore
                         let imageID = Vec.length images
                         writeStore . Vec.snoc images $ image
                         pure . Just $ imageID
    parseJSON v          = typeMismatch "ImageID" v
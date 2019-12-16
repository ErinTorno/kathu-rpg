{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Kathu.Graphics.Camera where

import Apecs
import GHC.Generics

newtype Camera = Camera {zoom :: Double} deriving (Show, Eq, Generic)

instance Component Camera where type Storage Camera = Unique Camera
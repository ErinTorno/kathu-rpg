{-# LANGUAGE DeriveGeneric #-}

module Kathu.Graphics.Camera where

import GHC.Generics

newtype Camera = Camera {zoom :: Double} deriving (Show, Eq, Generic)
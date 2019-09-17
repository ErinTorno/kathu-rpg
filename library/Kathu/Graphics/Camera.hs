{-# LANGUAGE DeriveGeneric #-}

module Kathu.Graphics.Camera where

import GHC.Generics

newtype Camera = Camera {zoom :: Float} deriving (Show, Eq, Generic)
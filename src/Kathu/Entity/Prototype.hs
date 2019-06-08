{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.Prototype where

import GHC.Generics
import Kathu.Entity.Components
import Kathu.Entity.PrototypeTemplate

-- We create an entity prototype that supports all given component types
defineData          "EntityPrototype" "" allSerialComponents
defineEntityCreator "newFromPrototype" "" allSerialComponents
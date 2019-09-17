{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- newFromPrototype is generated on-the-fly through TH without an explicit type signature

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MonoLocalBinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.Prototype where

import Data.Maybe

import Kathu.Entity.Components
import Kathu.Entity.PrototypeTemplate
import Kathu.Util.Types (Identifier)

-- We create an entity prototype that supports all given component types
defineData          "EntityPrototype"  "" serializableComponentConfigs
defineEntityCreator "newFromPrototype" "" serializableComponentConfigs

getPrototypeID :: EntityPrototype g -> Identifier
getPrototypeID = identifier . fromMaybe (error "Attempted to load entity without Identity") . identity

defineEntityFromJSON 'getPrototypeID ''EntityPrototype "" serializableComponentConfigs
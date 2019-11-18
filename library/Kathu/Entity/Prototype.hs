{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- newFromPrototype is generated on-the-fly through TH without an explicit type signature

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MonoLocalBinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.Prototype (EntityPrototype, newFromPrototype, getPrototypeID) where

import Data.Maybe

import Kathu.Entity.Components
import Kathu.Entity.Physics.BodyConfig (BodyConfig, setBodyConfig)
import Kathu.Entity.PrototypeTemplate
import Kathu.Util.Types (Identifier)

-- We create an entity prototype that supports all given component types
defineData          "EntityPrototype"  "" ((SerializableComponent ''BodyConfig False []):serializableComponentConfigs)
defineEntityCreator "newFromPrototypeSansPhysics" "" serializableComponentConfigs

-- this attaches the custom physics configuration type to our custom component's loadFromPrototype
newFromPrototype proto = newFromPrototypeSansPhysics proto >>= \ety -> (setBodyConfig ety . bodyConfig) proto >> pure ety

getPrototypeID :: EntityPrototype g -> Identifier
getPrototypeID = identifier . fromMaybe (error "Attempted to load entity without Identity") . identity

defineEntityFromJSON 'getPrototypeID ''EntityPrototype "" ((SerializableComponent ''BodyConfig False []):serializableComponentConfigs)
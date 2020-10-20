{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- newFromSimplePrototype is generated on-the-fly through TH without an explicit type signature

{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.Entity.Prototype (EntityPrototype(..), newFromSimplePrototype, getPrototypeID) where

import           Kathu.Entity.Components
import           Kathu.Entity.Physics.BodyConfig    (BodyConfig)
import           Kathu.Entity.PrototypeTemplate
import           Kathu.Entity.SerializableComponent
import           Kathu.Scripting.Lua.Types          (Script)
import           Verda.Util.Containers              (fromJustElseError)
import           Verda.Util.Types                   (Identifier)

defineData          "EntityPrototype" (SerializableComponent ''BodyConfig False [] : SerializableComponent ''Script True [] : serializableComponentConfigs)
defineEntityCreator "newFromSimplePrototype" serializableComponentConfigs

getPrototypeID :: EntityPrototype g -> Identifier
getPrototypeID = identifier . fromJustElseError "Attempted to load entity without Identity" . identity

defineEntityFromJSON 'getPrototypeID ''EntityPrototype (SerializableComponent ''BodyConfig False [] : SerializableComponent ''Script True [] : serializableComponentConfigs)
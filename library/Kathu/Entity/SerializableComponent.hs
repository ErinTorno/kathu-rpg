{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.SerializableComponent where

import Language.Haskell.TH.Syntax (Name)

import Kathu.Entity.ActorState
import Kathu.Entity.Components
import Kathu.Entity.Item          (Inventory)
import Kathu.Entity.LifeTime      (LifeTime)
import Kathu.Graphics.Drawable    (Render)

-- This class defines how different components will get serialized and deserialized

data SerializableComponent = SerializableComponent {compName :: Name, requiresDependencies :: Bool, params :: [String]}

serializableComponentConfigs :: [SerializableComponent]
serializableComponentConfigs =
    [ SerializableComponent {compName = ''Identity,      requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''SpecialEntity, requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''LifeTime,      requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''Tags,          requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''MovingSpeed,   requiresDependencies = False, params = []}
    , SerializableComponent {compName = ''Render,        requiresDependencies = True,  params = ["g"]}
    , SerializableComponent {compName = ''ActorState,    requiresDependencies = True,  params = []}
    , SerializableComponent {compName = ''Inventory,     requiresDependencies = True,  params = ["g"]}
    ]
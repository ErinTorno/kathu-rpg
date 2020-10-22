{-# LANGUAGE UndecidableInstances #-}

module Kathu.Entity.Prefab where

import           Data.Aeson
import           Data.Aeson.Types                (typeMismatch)
import           Verda.Graphics.Sprites          (SpriteID)
import           Verda.Parsing.Aeson
import           Verda.Util.Dependency
import           Verda.Util.Types                (Identifier, IDMap)

import           Kathu.Entity.ActorState         (ActorState)
import           Kathu.Entity.Components         (Identity, MovingSpeed, SpecialEntity, Tags, identifier)
import           Kathu.Entity.Item               (Inventory)
import           Kathu.Entity.LifeTime           (LifeTime)
import           Kathu.Entity.Physics.BodyConfig (BodyConfig)
import           Kathu.Graphics.Drawable         (Render)
import           Kathu.Scripting.Lua.Types       (Script)

data Prefab = Prefab
    { pIdentity      :: !Identity
    , pActorState    :: !(Maybe ActorState)
    , pBodyConfig    :: !(Maybe BodyConfig)
    , pInventory     :: !(Maybe (Inventory SpriteID))
    , pLifeTime      :: !(Maybe LifeTime)
    , pMovingSpeed   :: !(Maybe MovingSpeed)
    , pRender        :: !(Maybe (Render SpriteID))
    , pScript        :: !(Maybe Script)
    , pSpecialEntity :: !(Maybe SpecialEntity)
    , pTags          :: !(Maybe Tags)
    }

instance ( s `CanStore` IDMap Prefab
         , FromJSON (Dependency s m ActorState)
         , FromJSON (Dependency s m (Inventory SpriteID))
         , FromJSON (Dependency s m (Render SpriteID))
         , FromJSON (Dependency s m Script)
         , Monad m
         ) => FromJSON (Dependency s m Prefab) where
    parseJSON (Object v) = composeAndStoreWith prefabID $ Prefab
        <$> v .:^  "identity"
        <*> v .:-? "actor-state"
        <*> v .:^? "physics"
        <*> v .:-? "inventory"
        <*> v .:^? "life-time"
        <*> v .:^? "moving-speed"
        <*> v .:-? "render"
        <*> v .:-? "script"
        <*> v .:^? "special-entity"
        <*> v .:^? "tags"
    parseJSON e = typeMismatch "Prefab" e

prefabID :: Prefab -> Identifier
prefabID = identifier . pIdentity
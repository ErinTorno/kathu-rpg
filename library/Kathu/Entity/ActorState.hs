{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MonoLocalBinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.ActorState where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Lens
import Data.Functor.Compose
import qualified Data.HashMap.Strict as Hash
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import GHC.Generics
import Kathu.Entity.Damage
import Kathu.Entity.Resource
import Kathu.Parsing.Aeson
import Kathu.Util.Dependency

newtype Team = Team (Int) deriving (Show, Eq, Generic)

instance ToJSON Team where
    toJSON = genericToJSON standardProjectOptions
instance FromJSON Team where
    -- the following three are special cases
    parseJSON (String "ally")    = pure . Team $ 0
    parseJSON (String "enemy")   = pure . Team $ 1
    parseJSON (String "neutral") = pure . Team $ 2
    parseJSON (String "object")  = pure . Team $ 3
    parseJSON v = genericParseJSON standardProjectOptions v

data ActorState = ActorState
    { _team    :: Team
    , _health  :: Dynamic Float
    , _mana    :: Dynamic Float
    , _armor   :: Static Float
    , _aura    :: Static Float
    , _resists :: Map DamageID (Static Float)
    } deriving (Show, Eq, Generic)
makeLenses ''ActorState

instance (FromJSON (Dependency s m DamageID), Monad m) => FromJSON (Dependency s m ActorState) where
    parseJSON (Object v) = getCompose $ mkActor <*> Compose (parseMapDPWith parseJSON (fmap pure . parseJSON) $ v Hash.! "resists")
        where mkActor = ActorState
                  <$> v .:^ "team"
                  <*> v .:^ "health"
                  <*> v .:^ "mana"
                  <*> v .:^ "armor"
                  <*> v .:^ "aura"
    parseJSON v          = typeMismatch "ActorState" v

--------------------
-- Util functions --
--------------------

resistTo :: DamageProfile g -> ActorState -> Float
resistTo prof actor = maybe (defaultResist prof) total . (Map.lookup (dmgID prof)) $ actor^.resists

applyDamagePacket :: DamagePacket g -> ActorState -> ActorState
applyDamagePacket (DamagePacket prof res magn) =
    let modAv av actor = over av (modDynCur $ -1.0 * magn * (resistTo prof actor)) actor
    in case res of
        TgtHealth -> modAv health
        TgtMana   -> modAv mana
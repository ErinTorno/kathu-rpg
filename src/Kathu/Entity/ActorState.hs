{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.ActorState where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import GHC.Generics
import Kathu.Entity.Damage
import Kathu.Entity.Resource

newtype Team = Team (Int) deriving (Show, Eq, Generic)

data ActorState = ActorState
    { _team    :: Team
    , _health  :: Dynamic Float
    , _mana    :: Dynamic Float
    , _armor   :: Static Float
    , _aura    :: Static Float
    , _resists :: Map DamageID (Static Float)
    } deriving (Show, Eq, Generic)
makeLenses ''ActorState

resistTo :: DamageID -> ActorState -> Float
resistTo id actor = maybe 1.0 total . (Map.lookup id) $ actor^.resists

applyDamagePacket :: DamagePacket -> ActorState -> ActorState
applyDamagePacket (DamagePacket prof res magn) =
    let modAv av actor = over av (modDynCur $ -1.0 * magn * (resistTo (dmgID prof) actor)) actor
    in case res of
        TgtHealth -> modAv health
        TgtMana   -> modAv mana
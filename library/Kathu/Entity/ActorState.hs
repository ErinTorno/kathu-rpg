{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.Entity.ActorState where

import qualified Apecs
import           Data.Aeson
import           Data.Aeson.Types      (typeMismatch)
import           Control.Lens
import           Data.Functor.Compose
import qualified Data.HashMap.Strict   as Hash
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           GHC.Generics

import           Kathu.Entity.Damage
import           Kathu.Entity.Resource
import           Kathu.Parsing.Aeson
import           Kathu.Util.Dependency

newtype Team = Team Int deriving (Show, Eq, Generic)

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
    , _health  :: Dynamic Double
    , _mana    :: Dynamic Double
    , _armor   :: Static Double
    , _aura    :: Static Double
    , _resists :: Map DamageID (Static Double)
    } deriving (Show, Eq, Generic)
makeLenses ''ActorState

instance Apecs.Component ActorState where type Storage ActorState = Apecs.Map ActorState

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

resistTo :: DamageProfile g -> ActorState -> Double
resistTo prof actor = maybe (defaultResist prof) total . Map.lookup (dmgID prof) $ actor^.resists

applyDamagePacket :: DamagePacket g -> ActorState -> ActorState
applyDamagePacket (DamagePacket prof res magn) =
    let modAv av actor = over av (modDynCur $ (-1.0) * magn * resistTo prof actor) actor
    in case res of
        TgtHealth -> modAv health
        TgtMana   -> modAv mana
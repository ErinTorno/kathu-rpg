{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.Entity.Damage where

import Data.Aeson
import Data.Aeson.Types       (typeMismatch)
import Data.Functor.Compose   (getCompose)
import Data.Text              (Text)
import Data.Word

import GHC.Generics
import Kathu.Graphics.Color   (Color)
import Kathu.Parsing.Aeson
import Kathu.Parsing.Counting
import Kathu.Util.Dependency
import Kathu.Util.Flow        ((>>>=))
import Kathu.Util.Types       (Identifier, IDMap)

--------------
-- DamageID --
--------------

newtype DamageID = DamageID Word16 deriving (Show, Eq, Ord, Generic)

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m DamageID) where
    parseJSON (String s) = pure (DamageID . fromIntegral <$> lookupOrAdd "DamageID" s)
    parseJSON v          = typeMismatch "DamageID" v

--------------------------------
-- DamageProfile config types --
--------------------------------

data Defense = NoDefense | Armor | Aura deriving (Show, Eq)

instance ToJSON Defense where
    toJSON NoDefense = String "none"
    toJSON Armor     = String "armor"
    toJSON Aura      = String "aura"
instance FromJSON Defense where
    parseJSON (String "none")  = pure NoDefense
    parseJSON (String "armor") = pure Armor
    parseJSON (String "aura")  = pure Aura
    parseJSON v = typeMismatch "Defense" v

data DamageTarget = TgtHealth | TgtMana deriving (Show, Eq)

instance ToJSON DamageTarget where
    toJSON TgtHealth = String "health"
    toJSON TgtMana   = String "mana"
instance FromJSON DamageTarget where
    parseJSON (String "health") = pure TgtHealth
    parseJSON (String "mana")   = pure TgtMana
    parseJSON v = typeMismatch "DamageTarget" v

-------------------
-- DamageProfile --
-------------------

data DamageProfile g = DamageProfile
    { dmgID         :: DamageID
    , dmgTextID     :: Identifier
    , dmgName       :: Text
    , dmgIcon       :: g
    , dmgColor      :: Color
    , targetDefense :: Defense
    , defaultResist :: Double
    }

instance ( s `CanStore` IDMap (DamageProfile g)
         , FromJSON (Dependency s m DamageID)
         , FromJSON (Dependency s m g)
         , Monad m
         ) => FromJSON (Dependency s m (DamageProfile g)) where
    parseJSON (Object v) = damagePar >>>= (\dam -> dependencyMapInsert (dmgTextID dam) dam) >> damagePar
        where damagePar = getCompose $ DamageProfile
                  <$> v .:~ "damage-id"
                  <*> v .:^ "damage-id"
                  <*> v .:^ "name"
                  <*> v .:~ "icon"
                  <*> v .:^ "color"
                  <*> v .:^ "defense"
                  <*> v .:^? "default-resist" .!=~ 1.0
    parseJSON v          = typeMismatch "DamageProfile" v

data DamagePacket g = DamagePacket
    { damageProfile  :: !(DamageProfile g)
    , targetResource :: !DamageTarget
    , magnitude      :: !Double
    } 
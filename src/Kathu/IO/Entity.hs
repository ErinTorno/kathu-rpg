{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.IO.Entity where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Functor.Compose
import qualified Data.HashMap.Strict as Hash
import Kathu.Entity.ActorState
import Kathu.Entity.Damage
import Kathu.Entity.Resource
import Kathu.IO.Graphics
import Kathu.IO.Parsing
import Kathu.Util.Misc ((>>>=))

-- ActorState

instance FromJSON (SystemLink ActorState) where
    parseJSON (Object v) = getCompose $ mkActor <*> Compose (parseMapSLWith parseJSON (fmap pure . parseJSON) $ v Hash.! "resists")
        where mkActor = ActorState
                  <$> v .:^ "team"
                  <*> v .:^ "health"
                  <*> v .:^ "mana"
                  <*> v .:^ "armor"
                  <*> v .:^ "aura"
    parseJSON v          = typeMismatch "ActorState" v
    
instance ToJSON Team where
    toJSON = genericToJSON projectOptions
instance FromJSON Team where
    -- the following three are special cases
    parseJSON (String "ally") = pure . Team $ 0
    parseJSON (String "enemy") = pure . Team $ 1
    parseJSON (String "neutral") = pure . Team $ 2
    parseJSON (String "object") = pure . Team $ 3
    parseJSON v = genericParseJSON defaultOptions v

-- Damage

instance FromJSON (SystemLink DamageID) where
    parseJSON (String s) = pure $ (DamageID . fromIntegral) <$> lookupOrAdd "DamageID" s (mapInsertIncr s)
    parseJSON v          = typeMismatch "DamageID" v

instance ToJSON Defense where
    toJSON NoDefense = String "none"
    toJSON Armor     = String "armor"
    toJSON Aura      = String "aura"
instance FromJSON Defense where
    parseJSON (String "none")  = pure NoDefense
    parseJSON (String "armor") = pure Armor
    parseJSON (String "aura")  = pure Aura
    parseJSON v = typeMismatch "Defense" v

instance ToJSON DamageTarget where
    toJSON TgtHealth = String "health"
    toJSON TgtMana   = String "mana"
instance FromJSON DamageTarget where
    parseJSON (String "health") = pure TgtHealth
    parseJSON (String "mana")   = pure TgtMana
    parseJSON v = typeMismatch "DamageTarget" v

instance FromJSON (SystemLink DamageProfile) where
    parseJSON (Object v) = damagePar >>>= \dam -> insertSL damageProfiles (dmgTextID dam) dam
        where getID = lookupOrAdd "DamageProfile"
              damagePar = getCompose $ DamageProfile
                  <$> v .:~ "damage-id"
                  <*> v .:^ "damage-id"
                  <*> v .:^ "name"
                  <*> v .:~ "icon"
                  <*> v .:^ "color"
                  <*> v .:^ "defense"
                  <*> v .:^? "default-resist" .!=~ 1.0
    parseJSON v          = typeMismatch "DamageProfile" v
    
-- Resources
-- the resources can be loaded from either a single number (in which case the rest will be assumed), or from an object

instance ToJSON a => ToJSON (Static a) where
    toJSON (Static base bonus) = object ["base" .= base, "bonus" .= bonus]

    toEncoding (Static base bonus) = pairs ("base" .= base <> "bonus" .= bonus)

instance (Fractional a, FromJSON a) => FromJSON (Static a) where
    parseJSON (Object m) = Static <$> m .: "base" <*> m .: "bonus"
    parseJSON (Number s) = pure $ Static (fromScientific s) 0
    parseJSON e          = typeMismatch "Static" e

instance ToJSON a => ToJSON (Dynamic a) where
    toJSON (Dynamic cur base bonus) = object ["cur" .= cur, "base" .= base, "bonus" .= bonus]

    toEncoding (Dynamic cur base bonus) = pairs ("cur" .= cur <> "base" .= base <> "bonus" .= bonus)

instance (Fractional a, FromJSON a) => FromJSON (Dynamic a) where
    parseJSON (Object m) = Dynamic <$> m .: "cur" <*> m .: "base" <*> m .: "bonus"
    parseJSON (Number s) = let base = fromScientific s in pure $ Dynamic base base 0
    parseJSON e          = typeMismatch "Dynamic" e
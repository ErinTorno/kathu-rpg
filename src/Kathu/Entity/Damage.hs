{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Entity.Damage where

import Data.Text (Text)
import Data.Word
import GHC.Generics
import Kathu.Graphics.Color (Color)
import Kathu.Graphics.Drawable (RenderSprite)

newtype DamageID = DamageID Word16 deriving (Show, Eq, Ord, Generic)

data Defense = NoDefense | Armor | Aura deriving (Show, Eq)

data DamageTarget = TgtHealth | TgtMana deriving (Show, Eq)

data DamageProfile = DamageProfile
    { dmgID     :: DamageID
    , dmgTextID :: Text
    , dmgName   :: Text
    , dmgIcon   :: RenderSprite
    , dmgColor  :: Color
    , targetDefense :: Defense
    , defaultResist :: Float
    }

data DamagePacket = DamagePacket
    { damageProfile :: DamageProfile
    , targetResource :: DamageTarget
    , magnitude :: Float
    } 
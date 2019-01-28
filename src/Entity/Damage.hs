{-# LANGUAGE OverloadedStrings #-}

module Entity.Damage where

import Data.Text (Text)
import Graphics.Color (Color)
import qualified SDL

data Defense = NoDefense | Armor | Aura

data DamageProfile = DamageProfile
    { dmgName :: Text
    , dmgIcon :: SDL.Surface
    , dmgColor :: Color
    , targetDefense :: Defense
    }
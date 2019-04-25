{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.IO.Components where

import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Text (Text)
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Kathu.Entity.Components
import qualified Kathu.Entity.Resource as R
import Kathu.Entity.Prototype
import Kathu.Entity.System
import Kathu.Graphics.Drawable
import Kathu.IO.Graphics
import Kathu.IO.Misc
import Kathu.IO.Parsing
import Linear.V3 (V3(..))

-- The EntityPrototype itself

defineEntityFromJSON "EntityPrototype" "" allSerialComponents linkedSerialComponents

-- Resources
-- the resources can be loaded from either a single number (in which case the rest will be assumed), or from an object

instance ToJSON a => ToJSON (R.Static a) where
    toJSON (R.Static base bonus) = object ["base" .= base, "bonus" .= bonus]

    toEncoding (R.Static base bonus) = pairs ("base" .= base <> "bonus" .= bonus)

instance (Fractional a, FromJSON a) => FromJSON (R.Static a) where
    parseJSON (Object m) = R.Static <$> m .: "base" <*> m .: "bonus"
    parseJSON (Number s) = pure $ R.Static (fromScientific s) 0
    parseJSON e          = typeMismatch "Static" e

instance ToJSON a => ToJSON (R.Dynamic a) where
    toJSON (R.Dynamic cur base bonus) = object ["cur" .= cur, "base" .= base, "bonus" .= bonus]

    toEncoding (R.Dynamic cur base bonus) = pairs ("cur" .= cur <> "base" .= base <> "bonus" .= bonus)

instance (Fractional a, FromJSON a) => FromJSON (R.Dynamic a) where
    parseJSON (Object m) = R.Dynamic <$> m .: "cur" <*> m .: "base" <*> m .: "bonus"
    parseJSON (Number s) = let base = fromScientific s in pure $ R.Dynamic base base 0
    parseJSON e          = typeMismatch "Dynamic" e

-- Simple Components: need no custom instances

instance ToJSON MovingSpeed where
    toJSON = genericToJSON projectOptions
instance FromJSON MovingSpeed where
    parseJSON = genericParseJSON projectOptions

instance ToJSON Tags where
    toJSON = genericToJSON projectOptions
instance FromJSON Tags where
    parseJSON = genericParseJSON projectOptions


instance ToJSON ActorState where
    toJSON = genericToJSON projectOptions
instance FromJSON ActorState where
    parseJSON = genericParseJSON projectOptions

-- Complex Components: needs custom instances

instance ToJSON Identity where
    toJSON = genericToJSON projectOptions
instance FromJSON Identity where
    parseJSON (String s) = pure $ Identity s "" "" -- basic one with only an id
    parseJSON (Object v) = Identity <$> v .: "id" <*> v .:? "name" .!= "" <*> v .:? "description" .!= ""
    parseJSON e          = typeMismatch "Identity" e

instance ToJSON SpecialEntity where
    toJSON = genericToJSON projectOptions
instance FromJSON SpecialEntity where
    parseJSON (String s) = case T.toLower s of
        "player" -> pure Player
        _        -> fail $ "Couldn't match " ++ T.unpack s ++ " with any known instance of SpecialEntity"
    parseJSON e = typeMismatch "SpecialEntity" e

instance ToJSON Position where
    toJSON = genericToJSON projectOptions
instance FromJSON Position where
    parseJSON (String "default") = pure . Position $ V3 0 0 0
    parseJSON v = genericParseJSON defaultOptions v

instance ToJSON Velocity where
    toJSON = genericToJSON projectOptions
instance FromJSON Velocity where
    parseJSON (String "default") = pure . Velocity $ V3 0 0 0
    parseJSON v = genericParseJSON defaultOptions v

instance ToJSON Team where
    toJSON = genericToJSON projectOptions
instance FromJSON Team where
    -- the following three are special cases
    parseJSON (String "ally") = pure . Team $ 0
    parseJSON (String "enemy") = pure . Team $ 1
    parseJSON (String "neutral") = pure . Team $ 2
    parseJSON (String "object") = pure . Team $ 3
    parseJSON v = genericParseJSON defaultOptions v

instance FromJSON (SystemLink Render) where
    parseJSON obj@(Object v) = (\v -> v >>= pure . Render . Vec.singleton) <$> parseJSON obj
    parseJSON (Array a)      = toRender <$> Vec.foldM run (pure []) a
        where run :: SystemLink [RenderSprite] -> Value -> Parser (SystemLink [RenderSprite])
              run acc cur = (\rn -> rn >>= \inner -> (inner:) <$> acc) <$> parseJSON cur
              toRender ls = Render <$> (Vec.fromList <$> ls)
    parseJSON e              = typeMismatch "Render" e
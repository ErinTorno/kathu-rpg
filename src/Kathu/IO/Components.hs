{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.IO.Components where

import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Kathu.Entity.Components
import Kathu.Entity.Prototype
import Kathu.Entity.PrototypeTemplate
import Kathu.Graphics.Drawable
import Kathu.IO.Entity
import Kathu.IO.Graphics
import Kathu.IO.Item
import Kathu.IO.Misc
import Kathu.IO.Parsing
import Kathu.IO.ParsingLibrary
import Linear.V3 (V3(..))

-- The EntityPrototype itself

getPrototypeID = identifier . fromMaybe (error "Attempted to load entity without Identity") . identity

defineEntityFromJSON ''ParsingLibrary 'plEntities 'getPrototypeID "EntityPrototype" "" allSerialComponents linkedSerialComponents

-- Simple Components: need no custom instances

instance ToJSON MovingSpeed where
    toJSON = genericToJSON projectOptions
instance FromJSON MovingSpeed where
    parseJSON = genericParseJSON projectOptions

instance ToJSON Tags where
    toJSON = genericToJSON projectOptions
instance FromJSON Tags where
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

instance FromJSON (SystemLink' Render) where
    parseJSON obj@(Object v) = (\v -> v >>= pure . Render . Vec.singleton) <$> parseJSON obj
    parseJSON (Array a)      = toRender <$> Vec.foldM run (pure []) a
        where run :: SystemLink' [RenderSprite] -> Value -> Parser (SystemLink' [RenderSprite])
              run acc cur = (\rn -> rn >>= \inner -> (inner:) <$> acc) <$> parseJSON cur
              toRender ls = Render <$> (Vec.fromList <$> ls)
    parseJSON e              = typeMismatch "Render" e
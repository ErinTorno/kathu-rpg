{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.World where

import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import IO.Parsing
import World.Tile

-- these all use SystemLink, due to the conversion from Strings to IDs requiring state known

instance FromJSON (SystemLink TileID) where
    parseJSON (String s) = pure $ (TileID . fromIntegral) <$> lookupOrAdd tileIDs (\l -> l {tileIDs = mapInsertIncr s (tileIDs l)}) s
    parseJSON v          = typeMismatch "TileID" v

instance FromJSON (SystemLink ToolType) where
    parseJSON (String s) = pure $ ToolType <$> lookupOrAdd toolIDs (\l -> l {toolIDs = mapInsertIncr s (toolIDs l)}) s
    parseJSON v          = typeMismatch "ToolType" v

instance FromJSON (SystemLink BreakBehavior) where
    parseJSON (String "unbreakable") = pure . pure $ Unbreakable
    parseJSON (Object v) = getCompose $ Breakable <$> v .:~ "toolType" <*> v .:^ "minimumPower" <*> v .:^ "durability"
    parseJSON v          = typeMismatch "BreakBehavior" v
    
instance FromJSON (SystemLink Tile) where
    parseJSON (Object v) = getCompose $ Tile <$> v .:~ "tileID" <*> v .:^ "tileID" <*> v .:^ "name" <*> v .:~ "breakBehavior"
    parseJSON v          = typeMismatch "Tile" v

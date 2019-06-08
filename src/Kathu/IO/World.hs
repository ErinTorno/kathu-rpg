{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Kathu.IO.World where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Kathu.Entity.Item
import Kathu.IO.Components
import Kathu.IO.Item
import Kathu.IO.Graphics
import Kathu.IO.Parsing
import Kathu.IO.ParsingLibrary
import Kathu.Util.Misc ((>>>=))
import Kathu.Util.MultiDimVector (fromList3D)
import Kathu.World.Field
import Kathu.World.Tile
import Kathu.World.Time
import Kathu.World.WorldSpace
import Linear.V3 (V3(..))

-- these all use SystemLink', due to the conversion from Strings to IDs requiring state known

instance FromJSON (SystemLink' TileID) where
    parseJSON (String s) = pure $ (TileID . fromIntegral) <$> lookupOrAdd countingIDs "TileID" s (mapInsertIncr s)
    parseJSON v          = typeMismatch "TileID" v

instance FromJSON (SystemLink' ToolType) where
    parseJSON (String s) = pure $ ToolType <$> lookupOrAdd countingIDs "ToolID" s (mapInsertIncr s)
    parseJSON v          = typeMismatch "ToolType" v

instance FromJSON (SystemLink' BreakBehavior) where
    parseJSON (String "unbreakable") = pure . pure $ Unbreakable
    parseJSON (Object v) = getCompose $ Breakable <$> v .:~ "toolType" <*> v .:^ "minimumPower" <*> v .:^ "durability"
    parseJSON v          = typeMismatch "BreakBehavior" v
    
instance FromJSON (SystemLink' Tile) where
    parseJSON (Object v) = tilePar >>>= \tl -> insertSL plTiles (tl ^. tileTextID) tl
        where tilePar = getCompose $ Tile
                  <$> v .:~ "tile-id" -- this uses the id to parse SystemLink' TileID
                  <*> v .:^ "tile-id" -- this is used for the text name
                  <*> v .:^ "name"
                  <*> v .:~ "render"
                  <*> v .:^ "is-solid"
                  <*> v .:~ "break-behavior"
    parseJSON v          = typeMismatch "Tile" v


instance ToJSON DaylightConfig where
    toJSON = genericToJSON projectOptions
instance FromJSON DaylightConfig where
    parseJSON = genericParseJSON projectOptions

instance ToJSON TimeOfDay where
    toJSON Dawn      = String "dawn"
    toJSON Afternoon = String "afternoon"
    toJSON Dusk      = String "dusk"
    toJSON Night     = String "night"
instance FromJSON TimeOfDay where
    parseJSON (String "dawn")      = pure Dawn
    parseJSON (String "afternoon") = pure Afternoon
    parseJSON (String "dusk")      = pure Dusk
    parseJSON (String "night")     = pure Night
    parseJSON (String s)           = error . concat $ ["Attempted to parse time of day with invalid time \"", show s, "\""]
    parseJSON v = typeMismatch "TimeOfDay" v

----------------
-- WorldSpace --
----------------

instance FromJSON (SystemLink' WorldSpace) where
    parseJSON (Object v) = do
        id       <- v .: "world-id"
        name     <- v .: "name"
        palettes <- v .: "palettes"
        loadPnt  <- (*unitsPerTile) <$> v .: "load-point"
        layers :: [[[Char]]] <- v .: "layers"
        legend :: SystemLink' (Map Char Tile) <- do
            (keys, vals) :: ([Text], [Text]) <- ((unzip . Map.toList) <$> v .: "legend")
            pure . fmap (Map.fromList . zip (T.head <$> keys)) . lookupEach plTiles $ vals
        let parsePlacement :: (Value -> Parser (SystemLink' a)) -> Value -> Parser (SystemLink' [(V3 Float, a)])
            parsePlacement fn (Array vec) = foldM (parseIndivPlace fn) (pure []) vec
            parsePlacement _ v            = typeMismatch "Placement" v
            parseIndivPlace :: (Value -> Parser (SystemLink' a)) -> SystemLink' [(V3 Float, a)] -> Value -> Parser (SystemLink' [(V3 Float, a)])
            parseIndivPlace fn acc val@(Object v) = do
                pos   <- (*unitsPerTile) <$> v .: "position"
                stack <- fn val
                pure $ acc >>= \ls -> ((:ls) . (pos,)) <$> stack
            parseIndivPlace _ _ v                 = typeMismatch "Placement" v
            parseEty = withObject "EntityPlacement" $ \v -> v .: "entity" >>= pure . lookupSingle plEntities
        items <- v .: "items" >>= parsePlacement parseJSON >>>= pure . Vec.fromList
        entities <- v .: "entities" >>= parsePlacement parseEty >>>= pure . Vec.fromList
        let layersVec = (\lgnd -> fromList3D . fmap (fmap (fmap (fromMaybe fail . (flip Map.lookup) lgnd))) $ layers) <$> legend
            fail = error "Attempted to tile without a listing in the WorldSpace's legend"
        pure $ WorldSpace id name palettes loadPnt <$> entities <*> items <*> (liftSL . fromTileList =<< layersVec)
    parseJSON v          = typeMismatch "WorldSpace" v
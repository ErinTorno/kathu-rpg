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
import Kathu.Util.Misc ((>>>=))
import Kathu.Util.MultiDimVector (fromList3D)
import Kathu.World.Field
import Kathu.World.Tile
import Kathu.World.WorldSpace
import Linear.V3 (V3(..))

-- these all use SystemLink, due to the conversion from Strings to IDs requiring state known

instance FromJSON (SystemLink TileID) where
    parseJSON (String s) = pure $ (TileID . fromIntegral) <$> lookupOrAdd "TileID" s (mapInsertIncr s)
    parseJSON v          = typeMismatch "TileID" v

instance FromJSON (SystemLink ToolType) where
    parseJSON (String s) = pure $ ToolType <$> lookupOrAdd "ToolID" s (mapInsertIncr s)
    parseJSON v          = typeMismatch "ToolType" v

instance FromJSON (SystemLink BreakBehavior) where
    parseJSON (String "unbreakable") = pure . pure $ Unbreakable
    parseJSON (Object v) = getCompose $ Breakable <$> v .:~ "toolType" <*> v .:^ "minimumPower" <*> v .:^ "durability"
    parseJSON v          = typeMismatch "BreakBehavior" v
    
instance FromJSON (SystemLink Tile) where
    parseJSON (Object v) = tilePar >>>= \tl -> insertSL plTiles (tl ^. tileTextID) tl
        where tilePar = getCompose $ Tile
                  <$> v .:~ "tile-id" -- this uses the id to parse SystemLink TileID
                  <*> v .:^ "tile-id" -- this is used for the text name
                  <*> v .:^ "name"
                  <*> v .:~ "render"
                  <*> v .:^ "is-solid"
                  <*> v .:~ "break-behavior"
    parseJSON v          = typeMismatch "Tile" v

----------------
-- WorldSpace --
----------------

instance FromJSON (SystemLink WorldSpace) where
    parseJSON (Object v) = do
        id       <- v .: "world-id"
        palettes <- v .: "palettes"
        loadPnt  <- v .: "load-point"
        layers :: [[[Char]]] <- v .: "layers"
        legend :: SystemLink (Map Char Tile) <- do
            (keys, vals) :: ([Text], [Text]) <- ((unzip . Map.toList) <$> v .: "legend")
            pure . fmap (Map.fromList . zip (T.head <$> keys)) . lookupEach plTiles $ vals
        let parseItems :: Value -> Parser (SystemLink [(V3 Float, ItemStack)])
            parseItems (Array vec) = foldM parsePlacement (pure []) vec
            parseItems v           = typeMismatch "ItemPlacement" v
            parsePlacement :: SystemLink [(V3 Float, ItemStack)] -> Value -> Parser (SystemLink [(V3 Float, ItemStack)])
            parsePlacement acc val@(Object v) = do
                pos   <- (*unitsPerTile) <$> v .: "position"
                stack :: SystemLink ItemStack <- parseJSON val
                pure $ acc >>= \ls -> ((:ls) . (pos,)) <$> stack
            parsePlacement _ v                = typeMismatch "ItemPlacement" v
        items <- v .: "items" >>= parseItems >>>= pure . Vec.fromList
        let layersVec = (\lgnd -> fromList3D . fmap (fmap (fmap (fromMaybe fail . (flip Map.lookup) lgnd))) $ layers) <$> legend
            fail = error "Attempted to tile without a listing in the WorldSpace's legend"
        pure $ WorldSpace id palettes loadPnt <$> items <*> (liftSL . fromTileList =<< layersVec)
    parseJSON v          = typeMismatch "WorldSpace" v
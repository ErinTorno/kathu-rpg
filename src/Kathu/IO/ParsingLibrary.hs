{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.IO.ParsingLibrary where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Kathu.Entity.Damage (DamageProfile)
import Kathu.Entity.Item (Item)
import Kathu.Entity.Prototype
import Kathu.IO.Parsing
import Kathu.Graphics.Drawable
import Kathu.World.Tile (Tile, emptyTile)
import qualified SDL

type SystemLink' = SystemLink ParsingLibrary

data ParsingLibrary = ParsingLibrary
    { _plImages :: Vector Image
    , _countingIDs :: Map Text (Map Text Int) -- First key is category, second is individual id and associated index
    , _plEntities :: Map Text EntityPrototype
    , _plItems :: Map Text Item
    , _plTiles :: Map Text Tile
    , _damageProfiles :: Map Text DamageProfile
    , _workingDirectory :: String
    , _renderer :: SDL.Renderer
    }
makeLenses ''ParsingLibrary

mkEmptyPL :: SDL.Renderer -> ParsingLibrary
mkEmptyPL ren = ParsingLibrary
    { _plImages = Vec.empty
    , _countingIDs = Map.fromList [("TileID", Map.fromList [("empty", 0)])]
    , _plEntities = Map.empty
    , _plItems = Map.empty
    , _plTiles = Map.fromList [("empty", emptyTile)]
    , _damageProfiles = Map.empty
    , _workingDirectory = ""
    , _renderer = ren
    }
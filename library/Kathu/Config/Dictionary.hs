{-# LANGUAGE TemplateHaskell #-}

module Kathu.Config.Dictionary where

import           Control.Lens
import qualified Data.Map                        as Map
import           Data.Maybe
import qualified SDL
import           Verda.Graphics.Fonts            (Font, fontID)
import           Verda.Graphics.SpriteManager    (SpriteManager, mkSpriteManager)
import           Verda.IO.Dictionary
import           Verda.IO.Directory              (assetPath)
import           Verda.Util.Types

import           Kathu.Language
import           Kathu.Config.KathuStore
import           Kathu.Entity.Item
import           Kathu.Entity.Physics.Floor      (FloorProperty(..))
import           Kathu.Entity.Prefab             (Prefab, prefabID)
import           Kathu.Graphics.Palette          (Palette, paletteID)
import           Kathu.Graphics.UI
import           Kathu.World.Tile
import           Kathu.World.WorldSpace

data Dictionary = Dictionary
    { _dictParsingStore    :: !KathuStore
    , _dictItems           :: !(IDMap Item)
    , _dictFloorProperties :: !(IDMap FloorProperty)
    , _dictFonts           :: !(IDMap Font)
    , _dictLanguages       :: !(IDMap Language)
    , _dictPalettes        :: !(IDMap Palette)
    , _dictPrefabs         :: !(IDMap Prefab)
    , _dictTiles           :: !(IDMap Tile)
    , _dictUIConfig        :: UIConfig
    , _dictWorldSpaces     :: !(IDMap WorldSpace)
    }
makeLenses ''Dictionary

emptyDictionary :: Dictionary
emptyDictionary = Dictionary
    { _dictParsingStore    = emptyKathuStore
    , _dictItems           = emptyIDMap
    , _dictFloorProperties = emptyIDMap
    , _dictFonts           = emptyIDMap
    , _dictLanguages       = emptyIDMap
    , _dictPalettes        = emptyIDMap
    , _dictPrefabs         = emptyIDMap
    , _dictTiles           = emptyIDMap
    , _dictUIConfig        = error "Attempted to use UIConfig before it has been loaded"
    , _dictWorldSpaces     = emptyIDMap
    }

dictionaryLookup :: Dictionary -> Lens' Dictionary (IDMap a) -> Identifier -> Maybe a
dictionaryLookup !dict !getMap !key = Map.lookup key (dict^.getMap)

dictionaryFetch :: Dictionary -> Lens' Dictionary (IDMap a) -> Identifier -> a
dictionaryFetch !dict !getMap !key = fromMaybe err $ Map.lookup key (dict^.getMap)
    where err = error $ "Failed to retrieve dictionary value for key " ++ show key

loadDictionary :: SDL.Renderer -> IO (Dictionary, SpriteManager)
loadDictionary !renderer = do
    (dict, store) <- runDictionaryLoaders assetPath emptyDictionary emptyKathuStore
        [ parseFiles dictFonts           ".font"    fontID
        , parseFiles dictLanguages       ".lang"    langID
        , parseFiles dictPalettes        ".palette" paletteID
        , parseFiles dictItems           ".item"    itemID
        , parseFiles dictPrefabs         ".prefab"  prefabID
        , parseFiles dictFloorProperties ".floor"   propTextID
        , parseFiles dictTiles           ".tile"    (^.tileTextID)
        , parseFiles dictWorldSpaces     ".world"   (^.worldID)
        , parseUnique dictUIConfig "ui/config.hud"
        ]
    
    -- Construct SpriteManager for all loaded Surfaces
    manager <- mkSpriteManager renderer (store^.psSurfaces)
    let dict' = dict
            { _dictParsingStore = store
            , _dictTiles = Map.insert "empty" emptyTile (dict^.dictTiles)
            }
    -- All tiles must innately know of empty, since it isn't loaded through parsing
    pure (dict', manager)
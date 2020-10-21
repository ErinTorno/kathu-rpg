{-# LANGUAGE TemplateHaskell #-}

module Kathu.App.Data.Dictionary where

import           Control.Lens
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified SDL

import           Kathu.Language
import           Kathu.App.Data.KathuStore
import           Kathu.App.Graphics.Font    (Font)
import           Kathu.App.Graphics.Image   (ImageID)
import           Kathu.App.Graphics.ImageManager (ImageManager, mkImageManager)
import           Kathu.App.Graphics.UI
import           Kathu.Entity.Item
import           Kathu.Entity.Physics.Floor (FloorProperty(..))
import           Kathu.Entity.Prototype
import           Kathu.Graphics.Palette     (Palette, paletteID)
import           Kathu.World.Tile           hiding (Vector, MVector)
import           Kathu.World.WorldSpace
import           Verda.IO.Dictionary
import           Verda.IO.Directory         (assetPath)
import           Verda.Util.Types

data Dictionary = Dictionary
    { _dictParsingStore    :: !KathuStore
    , _dictItems           :: !(IDMap (Item ImageID))
    , _dictFloorProperties :: !(IDMap FloorProperty)
    , _dictLanguages       :: !(IDMap (Language Font))
    , _dictPalettes        :: !(IDMap Palette)
    , _dictPrototypes      :: !(IDMap (EntityPrototype ImageID))
    , _dictTiles           :: !(IDMap (Tile ImageID))
    , _dictUIConfig        :: UIConfig
    , _dictWorldSpaces     :: !(IDMap (WorldSpace ImageID))
    }
makeLenses ''Dictionary

emptyDictionary :: Dictionary
emptyDictionary = Dictionary
    { _dictParsingStore    = emptyKathuStore
    , _dictItems           = emptyIDMap
    , _dictFloorProperties = emptyIDMap
    , _dictLanguages       = emptyIDMap
    , _dictPalettes        = emptyIDMap
    , _dictPrototypes      = emptyIDMap
    , _dictTiles           = emptyIDMap
    , _dictUIConfig        = error "Attempted to use UIConfig before it has been loaded"
    , _dictWorldSpaces     = emptyIDMap
    }

dictionaryLookup :: Dictionary -> Lens' Dictionary (IDMap a) -> Identifier -> Maybe a
dictionaryLookup !dict !getMap !key = Map.lookup key (dict^.getMap)

dictionaryFetch :: Dictionary -> Lens' Dictionary (IDMap a) -> Identifier -> a
dictionaryFetch !dict !getMap !key = fromMaybe err $ Map.lookup key (dict^.getMap)
    where err = error $ "Failed to retrieve dictionary value for key " ++ show key

loadDictionary :: SDL.Renderer -> IO (Dictionary, ImageManager)
loadDictionary !renderer = do
    (dict, store) <- runDictionaryLoaders assetPath emptyDictionary emptyKathuStore
        [ parseFiles dictLanguages       ".lang"    langID
        , parseFiles dictPalettes        ".palette" paletteID
        , parseFiles dictItems           ".item"    itemID
        , parseFiles dictPrototypes      ".entity"  getPrototypeID
        , parseFiles dictFloorProperties ".floor"   propTextID
        , parseFiles dictTiles           ".tile"    (^.tileTextID)
        , parseFiles dictWorldSpaces     ".world"   (^.worldID)
        , parseUnique dictUIConfig "ui/config.hud"
        ]
    
    -- Construct SpriteManager for all loaded Surfaces
    manager <- mkImageManager renderer (store^.psImages)
    let dict' = dict
            { _dictParsingStore = store
            , _dictTiles = Map.insert "empty" emptyTile (dict^.dictTiles)
            }
    -- All tiles must innately know of empty, since it isn't loaded through parsing
    pure (dict', manager)
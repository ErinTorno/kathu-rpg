{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.App.Data.Library
    ( Library(..), images, uiConfig, prototypes, items, tiles, worldSpaces
    , loadLibrary
    ) where

import Control.Lens
import Data.Aeson
import qualified Data.Map as Map
import Data.Vector (Vector)

import Kathu.App.Data.KathuStore
import Kathu.App.Graphics.Image (Image, ImageID)
import Kathu.App.Graphics.UI
import Kathu.Entity.Components
import Kathu.Entity.Item
import Kathu.Entity.Prototype
import Kathu.IO.File (parseAllDP, parseExactlyNDP)
import Kathu.Util.Dependency
import Kathu.Util.Types (Identifier, IDMap)
import Kathu.World.Tile
import Kathu.World.WorldSpace

-- | This data type plays the role as a collection of named values for the game to read from when loading a level
data Library = Library
    { _images      :: Vector Image
    , _uiConfig    :: UIConfig
    , _prototypes  :: IDMap (EntityPrototype ImageID)
    , _items       :: IDMap (Item ImageID)
    , _tiles       :: IDMap (Tile ImageID)
    , _worldSpaces :: IDMap (WorldSpace ImageID)
    }
makeLenses ''Library

addEntities :: [EntityPrototype ImageID] -> Library -> Library
addEntities ety = set prototypes (etyMap ety)
    where etyMap = Map.fromList . fmap (\p -> (getID . identity $ p, p))
          getID (Just ident) = identifier ident
          getID Nothing      = error "Attempted to load entity from file without an id"

addAll :: Setter Library Library (IDMap a) (IDMap a) -> (a -> Identifier) -> [a] -> Library -> Library
addAll setter getKey elems = set setter map'
    where map' = Map.fromList . fmap (\e -> (getKey e, e)) $ elems

setImages :: (Library, KathuStore) -> IO (Library, KathuStore)
setImages (lib, plLib) = pure (set images (view plImages plLib) lib, plLib)

addUnique :: Setter Library Library a a -> [a] -> Library -> Library
addUnique setter (x:[]) lib = set setter x lib
addUnique _ _ _ = error "Attempted to add more than one items that are marked as unique"

loadLibrary :: Library -> FilePath -> IO Library
loadLibrary initialLibrary fldr = fst <$> process
    where parseDependency initState = (>>=(((flip runDependency) initState) . flattenDependency))
          -- parses all files of a type requiring Dependencies
          psDP :: FromJSON (Dependency KathuStore IO a) => (String, [a] -> Library -> Library) -> (Library, KathuStore) -> IO (Library, KathuStore) 
          psDP (ext, adder) (lib, plib) = (parseDependency plib . parseAllDP ext $ fldr) >>= \(nset, nplib) -> pure (adder nset lib, nplib)
          {-
          -- parses all files of a type without using Dependencies'
          psNo :: FromJSON a => (String, [a] -> Library -> Library) -> (Library, KathuStore) -> IO (Library, KathuStore)
          psNo (ext, adder) (lib, plib) = parseAll ext fldr >>= \nset -> pure (adder nset lib, plib)
          -}
          psUnique :: FromJSON (Dependency KathuStore IO a) => String -> Setter Library Library a a -> (Library, KathuStore) -> IO (Library, KathuStore) 
          psUnique ext setter (lib, plib) = (parseDependency plib . parseExactlyNDP 1 ext $ fldr) >>= \(nset, nplib) -> pure (addUnique setter nset lib, nplib)
          -- the set of elements to 
          start = (initialLibrary, emptyKathuStore)
          process = pure start
                >>= psDP ("item", addAll items itemID)
                >>= psDP ("entity", addEntities)
                >>= psDP ("tile", addAll tiles (view tileTextID))
                >>= psDP ("world", addAll worldSpaces worldID)
                >>= psUnique "ui" uiConfig
                >>= setImages
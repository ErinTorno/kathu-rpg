{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Kathu.App.Data.Library
    ( Library(..), uiConfig, prototypes, items, floorProperties, tiles, worldSpaces, font
    , loadLibrary
    ) where

import Control.Lens
import Data.Aeson
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified SDL as SDL
import qualified SDL.Font as SDLF

import Kathu.App.Data.KathuStore
import Kathu.App.Graphics.Image (ImageID)
import Kathu.App.Graphics.UI
import Kathu.Entity.Item
import Kathu.Entity.Physics.Floor (FloorProperty(..))
import Kathu.Entity.Prototype
import Kathu.IO.File (parseAllDP, parseExactlyNDP)
import Kathu.Util.Dependency
import Kathu.Util.Flow (mapSnd)
import Kathu.Util.Types (Identifier, IDMap)
import Kathu.World.Tile hiding (Vector, MVector)
import Kathu.World.WorldSpace

-- | This data type plays the role as a collection of named values for the game to read from when loading a level
data Library = Library
    { _uiConfig        :: UIConfig
    , _prototypes      :: IDMap (EntityPrototype ImageID)
    , _floorProperties :: IDMap FloorProperty
    , _items           :: IDMap (Item ImageID)
    , _tiles           :: IDMap (Tile ImageID)
    , _worldSpaces     :: IDMap (WorldSpace ImageID)
    , _font            :: SDLF.Font -- later on this should become a collection
    }
makeLenses ''Library

addEntities :: [EntityPrototype ImageID] -> Library -> Library
addEntities ety = set prototypes (etyMap ety)
    where etyMap = Map.fromList . fmap (\p -> (getPrototypeID p, p))

addAll :: Setter Library Library (IDMap a) (IDMap a) -> (a -> Identifier) -> [a] -> Library -> Library
addAll setter getKey elems = set setter map'
    where map' = Map.fromList . fmap (\e -> (getKey e, e)) $ elems

addUnique :: Setter Library Library a a -> [a] -> Library -> Library
addUnique setter (x:[]) lib = set setter x lib
addUnique _ _ _ = error "Attempted to add more than one items that are marked as unique"

-- once languages are implemented, the .lang files should configure font paths, not this
loadFonts :: (Library, KathuStore) -> IO (Library, KathuStore)
loadFonts (lib, store) = (,store) . (flip (set font)) lib <$> SDLF.load "assets/fonts/VT323-Regular.ttf" 28

-- Surfaces are not stored in the library, as once ImageManager is done doing conversions we want to GC it
loadLibrary :: Library -> FilePath -> IO (Library, Vector SDL.Surface)
loadLibrary initialLibrary fldr = process
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
          process :: IO (Library, Vector SDL.Surface)
          process = pure start
                >>= psDP ("item",   addAll items itemID)
                >>= psDP ("entity", addEntities)
                >>= psDP ("floor",  addAll floorProperties propTextID)
                >>= psDP ("tile",   addAll tiles (view tileTextID))
                -- All tiles must innately know of empty, since it isn't loaded through parsing
                >>= \(library, store) -> pure (over tiles (Map.insert "empty" emptyTile) library, store)
                >>= psDP ("world",  addAll worldSpaces worldID)
                >>= psUnique "hud"  uiConfig
                >>= loadFonts
                >>= pure . mapSnd (view plImages)
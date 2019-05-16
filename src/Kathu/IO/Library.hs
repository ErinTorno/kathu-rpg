{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Kathu.IO.Library (loadLibrary) where

import Control.Lens
import Control.Monad (foldM)
import Data.Aeson
import Data.Maybe
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Kathu.Entity.Components
import Kathu.Entity.Item
import Kathu.Entity.System
import Kathu.IO.Components
import Kathu.IO.File
import Kathu.IO.Graphics
import Kathu.IO.Parsing
import Kathu.IO.World
import Kathu.World.Tile
import Kathu.World.WorldSpace
import qualified SDL
import System.FilePath

addEntities :: [EntityPrototype] -> Library -> Library
addEntities ety = set prototypes (etyMap ety)
    where etyMap = Map.fromList . fmap (\p -> (getID . identity $ p, p))
          getID (Just ident) = identifier ident
          getID Nothing      = error "Attempted to load entity from file without an id"

addAll :: Setter Library Library (Map Text a) (Map Text a) -> (a -> Text) -> [a] -> Library -> Library
addAll setter getKey elems = set setter map'
    where map' = Map.fromList . fmap (\e -> (getKey e, e)) $ elems

setImages :: (Library, ParsingLibrary) -> IO (Library, ParsingLibrary)
setImages (lib, plLib) = pure (set images (view plImages plLib) lib, plLib)

addUnique :: Setter Library Library a a -> [a] -> Library -> Library
addUnique setter (x:[]) lib = set setter x lib
addUnique _ _ _ = error "Attempted to add more than one items that are marked as unique"

loadLibrary :: SDL.Renderer -> FilePath -> IO Library
loadLibrary renderer fldr = fst <$> process
    where -- parses all files of a type requiring SystemLink
          psSL :: FromJSON (SystemLink a) => (String, [a] -> Library -> Library) -> (Library, ParsingLibrary) -> IO (Library, ParsingLibrary) 
          psSL (ext, adder) (lib, plib) = (parseSL plib . parseAllSL ext $ fldr) >>= \(nset, nplib) -> pure (adder nset lib, nplib)
          -- parses all files of a type without using SystemLink
          psNo :: FromJSON a => (String, [a] -> Library -> Library) -> (Library, ParsingLibrary) -> IO (Library, ParsingLibrary)
          psNo (ext, adder) (lib, plib) = parseAll ext fldr >>= \nset -> pure (adder nset lib, plib)
          psUnique :: FromJSON (SystemLink a) => String -> Setter Library Library a a -> (Library, ParsingLibrary) -> IO (Library, ParsingLibrary) 
          psUnique ext setter (lib, plib) = (parseSL plib . parseExactlyNSL 1 ext $ fldr) >>= \(nset, nplib) -> pure (addUnique setter nset lib, nplib)
          -- the set of elements to 
          start = (mempty, mkEmptyPL renderer)
          process = pure start
                    >>= psSL ("entity", addEntities)
                    >>= psSL ("item", addAll items itemID)
                    >>= psSL ("tile", addAll tiles (view tileTextID))
                    >>= psSL ("world", addAll worldSpaces worldID)
                    >>= psUnique "ui" uiConfig
                    >>= setImages
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

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
import Kathu.Entity.System
import Kathu.IO.Components
import Kathu.IO.File
import Kathu.IO.Parsing
import Kathu.IO.World
import Kathu.World.Tile
import qualified SDL
import System.FilePath

addEntities :: [EntityPrototype] -> Library -> Library
addEntities ety lib = lib {prototypes = etyMap ety}
    where etyMap = Map.fromList . fmap (\p -> (getID . identity $ p, p))
          getID (Just ident) = identifier ident
          getID Nothing      = error "Attempted to load entity from file without an id"

addTiles :: [Tile] -> Library -> Library
addTiles tls lib = lib {tiles = Map.fromList $ (\t -> (view tileTextID t, t)) <$> tls}

loadLibrary :: SDL.Renderer -> FilePath -> IO Library
loadLibrary renderer fldr = fst <$> process
    where -- parses all files of a type requiring SystemLink
          psSL :: FromJSON (SystemLink a) => (String, [a] -> Library -> Library) -> (Library, ParsingLibrary) -> IO (Library, ParsingLibrary) 
          psSL (ext, adder) (lib, plib) = (parseSL plib . parseAllSL ext $ fldr) >>= \(nset, nplib) -> pure (adder nset lib, nplib)
          -- parses all files of a type without using SystemLink
          psNo :: FromJSON a => (String, [a] -> Library -> Library)  -> (Library, ParsingLibrary) -> IO (Library, ParsingLibrary)
          psNo (ext, adder) (lib, plib) = parseAll ext fldr >>= \nset -> pure (adder nset lib, plib)
          -- the set of elements to 
          start = (mempty, mkEmptyPL renderer)
          process = pure start
                    >>= psSL ("entity", addEntities)
                    >>= psSL ("tile", addTiles)
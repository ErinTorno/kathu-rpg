{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module IO.Library (loadLibrary) where

import Control.Monad (foldM)
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath

import Entity.Components
import Entity.System
import IO.Components
import IO.File
import IO.Parsing

addEntities :: [EntityPrototype] -> Library -> Library
addEntities ety lib = lib {prototypes = etyMap ety}
    where etyMap = Map.fromList . fmap (\p -> (getID . identity $ p, p))
          getID (Just ident) = identifier ident
          getID Nothing      = error "Attempted to load entity from file without an id"

loadLibrary :: FilePath -> IO Library
loadLibrary fldr = fst <$> process
    where -- parses all files of a type requiring SystemLink
          psSL :: FromJSON (SystemLink a) => (String, [a] -> Library -> Library) -> (Library, ParsingLibrary) -> IO (Library, ParsingLibrary) 
          psSL (ext, adder) (lib, plib) = (parseSL plib . parseAllSL ext $ fldr) >>= \(nset, nplib) -> pure (adder nset lib, nplib)
          -- parses all files of a type without using SystemLink
          psNo :: FromJSON a => (String, [a] -> Library -> Library)  -> (Library, ParsingLibrary) -> IO (Library, ParsingLibrary)
          psNo (ext, adder) (lib, plib) = parseAll ext fldr >>= \nset -> pure (adder nset lib, plib)
          -- the set of elements to 
          start = (emptyLibrary, emptyPL)
          process = pure start >>= psSL ("entity", addEntities)
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Kathu.IO.File where

import           Control.Monad              (liftM2, mapM)
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Text            (encodeToLazyText)
import qualified Data.Bifunctor             as Bi
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  (isPrefixOf)
import           Data.Text.Lazy.IO          as IL
import qualified Data.Yaml                  as Y
import           System.Directory
import           System.FilePath

import           Kathu.IO.Directory
import           Kathu.Util.Dependency
import           Kathu.Util.Flow            (partitionM)

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

data Format = FormatJSON | FormatYAML

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

loadError :: String -> FilePath -> a
loadError file = error . (++) (file ++ " | ")

loadWithHandlers :: FromJSON a => (String -> b) -> (a -> b) -> FilePath -> IO b
loadWithHandlers onFail onSuccess filepath
    | typePrefix "~json" = handle eitherDecode
    | otherwise          = handle (Bi.first show . Y.decodeEither' . toStrict) -- we default to yaml when nothing matches
    where -- files types are put into the path, in the format of "filename~FORMAT.ext"
          typePrefix c   = c `isPrefixOf` (takeBaseName filepath)
          handle decoder  = either onFail onSuccess . decoder <$> BL.readFile filepath

loadFromFile :: FromJSON a => FilePath -> IO a
loadFromFile fp = loadWithHandlers (loadError fp) id fp

maybeLoad :: FromJSON a => FilePath -> IO (Maybe a)
maybeLoad = loadWithHandlers (const Nothing) Just

saveToFile :: ToJSON a => Format -> FilePath -> a -> IO ()
saveToFile FormatJSON fd config = IL.writeFile fd (encodeToLazyText config)
saveToFile FormatYAML fd config = B.writeFile fd (Y.encode config)

---------------------
-- Parsing Related --
---------------------

-- for all files in directory and subdirectory with the given extension, we parse them and return it as a list
parseAllWith :: (FilePath -> IO a) -> String -> FilePath -> IO [a]
parseAllWith loader ext path = ((map (path </>)) <$> listDirectory path) >>= partitionM doesDirectoryExist >>= parseIn
    where -- parses each file, and appends to all parsed files from subdir
          parseIn (dirs, files) = (liftM2 (++)) foldRes . mapM loader . filter (isExtensionOf ext) $ files
              where foldRes = concat <$> foldM (\acc d -> (:acc) <$> parseAllWith loader ext d) [] dirs

parseAll :: FromJSON a => String -> FilePath -> IO [a]
parseAll = parseAllWith loadFromFile

loadFromFileDP :: (s `CanStore` WorkingDirectory, FromJSON (Dependency s m a), Monad m) => FilePath -> IO (Dependency s m a)
loadFromFileDP file = loadWithHandlers (loadError file) (modWDir>>) file
    where modWDir = writeStore (WorkingDirectory . takeDirectory $ file)

parseAllDP :: (s `CanStore` WorkingDirectory, FromJSON (Dependency s m a), Monad m) => String -> FilePath -> IO [Dependency s m a]
parseAllDP = parseAllWith loadFromFileDP

parseExactlyNDP :: (s `CanStore` WorkingDirectory, FromJSON (Dependency s m a), Monad m) => Int -> String -> FilePath -> IO [Dependency s m a]
parseExactlyNDP n s = fmap (\ls -> let len = length ls in if len /= n then failFor len else ls) . parseAllDP s
    where failFor len = error . concat $ ["Attempted to parse ", show n, " for file type .", show s, ", but found ", show len]
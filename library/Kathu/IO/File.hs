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
import           Data.List                  (isSuffixOf)
import           Data.Text.Lazy.IO          as IL
import qualified Data.Yaml                  as Y
import           System.Directory
import           System.FilePath

import           Kathu.IO.Directory
import qualified Kathu.Parsing.Yaml         as Yaml
import           Kathu.Util.Dependency
import           Kathu.Util.Flow            (partitionM)

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

data Format = FormatJSON | FormatYAML deriving (Show, Eq)

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

loadError :: String -> FilePath -> a
loadError file = error . (++) (file ++ " | ")

-- | Files types can be put into the path, in the format of "filename~FORMAT.ext"
-- | If no format is given, it is assumed to use YAML
fileFormatPrefix :: FilePath -> Format
fileFormatPrefix filepath
    | "~json" `isSuffixOf` takeBaseName filepath = FormatJSON
    | otherwise                                  = FormatYAML

loadWithHandlers :: FromJSON a => (String -> b) -> (a -> b) -> FilePath -> IO b
loadWithHandlers onFail onSuccess filepath = case fileFormatPrefix filepath of
    FormatJSON -> handle eitherDecode
    FormatYAML -> handle (Bi.first show . Y.decodeEither' . toStrict) -- we default to yaml when nothing matches
    where handle decoder  = either onFail onSuccess . decoder <$> BL.readFile filepath

loadFromFile :: FromJSON a => FilePath -> IO a
loadFromFile fp = loadWithHandlers (loadError fp) id fp

maybeLoad :: FromJSON a => FilePath -> IO (Maybe a)
maybeLoad = loadWithHandlers (const Nothing) Just

saveToFile :: ToJSON a => Format -> FilePath -> a -> IO ()
saveToFile FormatJSON fd val = IL.writeFile fd (encodeToLazyText val)
saveToFile FormatYAML fd val = B.writeFile fd (Yaml.encodeYaml val)

saveYamlToFileWithFieldOrder :: ToJSON a => Yaml.FieldOrder -> FilePath -> a -> IO ()
saveYamlToFileWithFieldOrder fieldOrd fd val = B.writeFile fd (Yaml.encodeYamlWithFieldOrder fieldOrd val)

---------------------
-- Parsing Related --
---------------------

-- for all files in directory and subdirectory with the given extension, we parse them and return it as a list
parseAllWith :: (FilePath -> IO a) -> String -> FilePath -> IO [a]
parseAllWith loader ext path = map (path </>) <$> listDirectory path >>= partitionM doesDirectoryExist >>= parseIn
    where -- parses each file, and appends to all parsed files from subdir
          parseIn (dirs, files) = liftM2 (++) foldRes . mapM loader . filter (isExtensionOf ext) $ files
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
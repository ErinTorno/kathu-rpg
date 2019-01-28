{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.File where

import Control.Monad (liftM2, mapM)
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.Either
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.IO as IL
import qualified Data.Yaml as Y
import System.Directory
import System.FilePath

import Util

assetPath :: FilePath
assetPath = "./assets"

-- Strings that begin with / are absolute, otherwise they are relative to the current directory
-- as such, using C:\\ or similar notations to access outside of the directory is not allowed
resolveAssetPath :: FilePath -> FilePath -> FilePath
resolveAssetPath p ('/':cs) = concat [assetPath, "/", cs]
resolveAssetPath "" rem     = concat [assetPath, "/", rem]
resolveAssetPath p rem | assetPath `isPrefixOf` p = concat [p, "/", rem]
                       | otherwise                = concat [assetPath, "/", p, "/", rem]

-- Loading

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

data Format = FormatJSON | FormatYAML

{-
-- the file can specify its format in the extension with use of a char
-- ex: myfile.y#txt is a txt file in the yml format
formatFromFD :: FileDescriptor -> Maybe Format
formatFromFD (FileDescriptor _ name) = 
    let (name, fullExt) = T.breakOnEnd "." name in
    case T.breakOn "#" of
        ("j", _) -> Just FormatJSON
        ("y", _) -> Just FormatYAML
        (n, _)   -> Nothing
-}

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

loadError file = error . (++) (file ++ " | ")

-- we tell json vs yaml by checking if first char is { or [. In yaml this will only happen if the key is a list or object, which we never plan on doing
loadWithHandlers :: FromJSON a => (String -> b) -> (a -> b) -> FilePath -> IO b
loadWithHandlers onFail onSuccess fp = (BL.readFile fp) >>= \d -> pure $ decode (CL8.head d) d
    where decode t d | t == '{' || t == '[' = either (onFail . show) onSuccess (eitherDecode d)
                     | otherwise            = either (onFail . show) onSuccess (Y.decodeEither' . toStrict $ d)

loadFromFile :: FromJSON a => FilePath -> IO a
loadFromFile fp = loadWithHandlers (loadError fp) id fp

maybeLoad :: FromJSON a => FilePath -> IO (Maybe a)
maybeLoad = loadWithHandlers (\_ -> Nothing) Just

saveToFile :: ToJSON a => Format -> FilePath -> a -> IO ()
saveToFile FormatJSON fd config = IL.writeFile fd (encodeToLazyText config)
saveToFile FormatYAML fd config = B.writeFile fd (Y.encode config)

-- for all files in directory and subdirectory with the given extension, we parse them and return it as a list
--parseAllWith :: FromJSON a => (FilePath -> IO a) -> String -> FilePath -> IO [a]
parseAllWith loader ext path = ((map (path </>)) <$> listDirectory path) >>= partitionM doesDirectoryExist >>= parseIn
    where -- parses each file, and appends to all parsed files from subdir
          parseIn (dirs, files) = (liftM2 (++)) foldRes . mapM loader . filter (isExtensionOf ext) $ files
              where foldRes = concat <$> foldM (\acc d -> (:acc) <$> parseAllWith loader ext d) [] dirs

parseAll :: FromJSON a => String -> FilePath -> IO [a]
parseAll = parseAllWith loadFromFile
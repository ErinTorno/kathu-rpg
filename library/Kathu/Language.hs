{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Kathu.Language where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.Foldable          as F
import           Data.Functor.Compose
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Map.Merge.Strict  (merge, preserveMissing, zipWithMatched)
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vec

import           Kathu.IO.File          (parseAll)
import           Kathu.IO.Directory
import           Kathu.Parsing.Aeson
import           Kathu.Util.Dependency
import           Kathu.Util.Types

data LangUnit = LangUnit
    { luValue :: !Text            -- the default value for this part of speech
    , luTags  :: !(Vector Text)   -- innate attributes associated with this part of speech
    , luCases :: !(Map Text Text) -- different variations of the value for different situations (such as grammatical case or gender)
    }

instance FromJSON LangUnit where
    parseJSON (String s) = pure $ LangUnit s Vec.empty Map.empty
    parseJSON (Object v) = LangUnit <$> v .: "default" <*> v .: "tags" <*> v .: "cases"
    parseJSON e          = typeMismatch "LangUnit" e

-- Category -> ID -> Key -> Text
-- i.e. "item" -> "sword" -> "name" -> "silver sword"
newtype Lines = Lines {unLines :: IDMap (IDMap (IDMap LangUnit))} deriving (FromJSON)

mergeLines :: [Lines] -> Lines
mergeLines = Lines . F.foldl' merger Map.empty . fmap unLines
    where merger   = merge preserveMissing preserveMissing (recZip (recZip takeFst))
          recZip n = zipWithMatched $ \_ -> merge preserveMissing preserveMissing n
          takeFst  = zipWithMatched $ \_ x _ -> x

-- ex:
-- Word with cases, plurals, and both
-- LangUnit "kato" $ Map.fromList [("akuzativa", "katon"), ("plurala", "katoj"), ("akuzativplurala", "katojn")]

data Language f = Language
    { langID    :: !Identifier
    , langName  :: !Text
    , langFonts :: IDMap f
    , langLines :: Lines
    }

instance
    ( s `CanProvide` WorkingDirectory
    , FromJSON (Dependency s m f)
    , MonadIO m
    ) => FromJSON (Dependency s m (Language f)) where
    parseJSON (Object m) = getCompose $ Language
        <$> m .:^ "lang-id"
        <*> m .:^ "name"
        <*> Compose (parseMapDP =<< m .: "fonts")
        <*> Compose (loadLines <$> m .:? "directory")
        where -- we load all .lines files in the given directory as text lines for this language
              loadLines (Just path) = resolveAssetPathDP path
                                  >>= liftDependency . liftIO . fmap mergeLines . parseAll "lines"
              loadLines Nothing     = pure (Lines Map.empty)
    parseJSON e          = typeMismatch "Language" e
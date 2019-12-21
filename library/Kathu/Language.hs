{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Kathu.Language where

import           Data.Aeson
import           Data.Aeson.Types      (typeMismatch)
import           Data.Functor.Compose
import           Data.Map              (Map)
import           Data.Text             (Text)

import           Kathu.Parsing.Aeson
import           Kathu.Util.Dependency
import           Kathu.Util.Types

data LangUnit = LangUnit
    { luValue :: !Text            -- the default value for this part of speech
    --, luTags  :: !(Vector Text) -- innate attributes associated with this part of speech
    , luCases :: !(Map Text Text) -- different variations of the value for different situations (such as grammatical case or gender)
    }

-- ex:
-- Word with cases, plurals, and both
-- LangUnit "kato" $ Map.fromList [("akuzativa", "katon"), ("plurala", "katoj"), ("akuzativplurala", "katojn")]

data Language f = Language
    { langID    :: !Identifier
    , langName  :: !Text
    , langFonts :: !(IDMap f)
    }

instance (FromJSON (Dependency s m f), Monad m) => FromJSON (Dependency s m (Language f)) where
    parseJSON (Object m) = getCompose $ Language
                       <$> m .:^ "lang-id"
                       <*> m .:^ "name"
                       <*> Compose (parseMapDP =<< m .: "fonts")
    parseJSON e = typeMismatch "Language" e
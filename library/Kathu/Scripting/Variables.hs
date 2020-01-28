{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Scripting.Variables where

import           Data.Aeson
import           Data.Aeson.Types     (Parser, typeMismatch)
import           Data.Int
import           Data.Serialize
import           Data.Text            (Text)
import           GHC.Generics

import           Kathu.Util.Types     (IDHashTable)

-- | Config-defined variables accessabled by scripts; these are serialized and saved with the game's state, unlike script instance variables
data WorldVariable
    = WorldBool   !Bool
    | WorldDouble !Double
    | WorldInt    !Int64
    | WorldText   !Text
    deriving (Show, Eq, Generic)

instance Serialize WorldVariable

instance ToJSON WorldVariable where
    toJSON (WorldBool b)   = object ["type" .= ("bool"   :: Text), "value" .= b]
    toJSON (WorldDouble d) = object ["type" .= ("double" :: Text), "value" .= d]
    toJSON (WorldInt i)    = object ["type" .= ("int"    :: Text), "value" .= i]
    toJSON (WorldText t)   = object ["type" .= ("text" :: Text),   "value" .= t]

instance FromJSON WorldVariable where
    parseJSON (Object v)   = v .: "type" >>= var
        where var :: Text -> Parser WorldVariable
              var "bool"   = WorldBool   <$> v .:? "value" .!= False
              var "double" = WorldDouble <$> v .:? "value" .!= 0
              var "int"    = WorldInt    <$> v .:? "value" .!= 0
              var "text"   = WorldText   <$> v .:? "value" .!= ""
              var e        = fail ("Unable to parse world variable with type of " ++ show e)
    parseJSON e            = typeMismatch "WorldVariable" e

----------------
-- Components --
----------------

data Variables = Variables
    { globalVariables     :: !(IDHashTable WorldVariable)
    , worldspaceVariables :: !(IDHashTable WorldVariable)
    }
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Kathu.World.Stasis where

import qualified Control.Monad.Fail   as F
import           Data.Aeson
import           Data.Aeson.Types     (Parser, typeMismatch)
import           Data.Int
import           Data.Serialize
import           Data.Text            (Text)
import qualified Data.Vector.Storable as SVec
import           GHC.Generics
import           Linear.V2            (V2(..))

import           Kathu.Util.Types     (IDMap)

---------------
-- Variables --
---------------

data WorldVariable
    = WorldBool   Bool
    | WorldDouble Double
    | WorldInt    Int64
      deriving (Show, Eq, Generic)

instance Serialize WorldVariable

instance ToJSON WorldVariable where
    toJSON (WorldBool b)   = object ["type" .= ("bool"   :: Text), "value" .= b]
    toJSON (WorldDouble d) = object ["type" .= ("double" :: Text), "value" .= d]
    toJSON (WorldInt i)    = object ["type" .= ("int"    :: Text), "value" .= i]
instance FromJSON WorldVariable where
    parseJSON (Object v)   = (v .: "type" :: Parser Text) >>= var
        where var "bool"   = WorldBool   <$> v .:? "value" .!= False
              var "double" = WorldDouble <$> v .:? "value" .!= 0
              var "int"    = WorldInt    <$> v .:? "value" .!= 0
              var e        = F.fail ("Unable to parse world variable with type of " ++ show e)
    parseJSON e            = typeMismatch "WorldVariable" e

------------
-- Stasis --
------------

-- | An object describing saved state information about a WorldSpace while it is inactive
data WorldStasis = WorldStasis
    { statisVariables  :: IDMap WorldVariable -- The world's variables
    -- Where the player entered the world from so we can reset them back to it
    -- Optionally, the player's actual location if the world supports it
    , playerEntryPoint :: V2 Double
    , removedEntities  :: SVec.Vector Int32     -- The index IDs of entities that are to be removed from the world's entity vector before spawning
    , removedItems     :: SVec.Vector Int32     -- Same but for items
    --, leftItems       :: ?
    }

worldStasisVersion :: Int32
worldStasisVersion = -1 -- unstable, no guarantee of backwards compatibility until >= 0

instance Serialize WorldStasis where
    put (WorldStasis vars ep remEtys remItems) = put worldStasisVersion >> put vars >> put ep >> put remEtys >> put remItems
    get = (get :: Get Int32) >>= ver
        where ver v | v == worldStasisVersion = WorldStasis <$> get <*> get <*> get <*> get
                    | otherwise               = F.fail ("Unknown WorldStasis Version " ++ show v)

------------
-- Stases --
------------

-- | A global component used for accessing the stases of hibernating WorldSpaces
newtype WorldStases = WorldStases {unStases :: IDMap WorldStasis} deriving Serialize
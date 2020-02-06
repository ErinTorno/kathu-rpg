{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Scripting.Variables where

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.ST        (stToIO)
import           Data.Aeson
import           Data.Aeson.Types        (Parser, typeMismatch)
import qualified Data.HashTable.ST.Basic as HT
import           Data.Int
import qualified Data.Map                as Map
import           Data.Serialize
import           Data.Text               (Text)
import           GHC.Generics

import           Kathu.Util.Types        (Identifier, IDHashTable, IDMap)

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

initialVariablesSize :: Int
initialVariablesSize = 32

data Variables = Variables
    { globalVariables     :: !(IDHashTable WorldVariable)
    , worldspaceVariables :: !(IDHashTable WorldVariable)
    }

initVariables :: MonadIO m => m Variables
initVariables = do
    globalHT <- liftIO . stToIO $ HT.newSized initialVariablesSize
    worldHT  <- liftIO . stToIO $ HT.newSized initialVariablesSize
    return (Variables globalHT worldHT)

getGlobalVariable :: MonadIO m => Identifier -> Variables -> m (Maybe WorldVariable)
getGlobalVariable idt (Variables ht _) = liftIO . stToIO $ HT.lookup ht idt

getWorldVariable :: MonadIO m => Identifier -> Variables -> m (Maybe WorldVariable)
getWorldVariable idt (Variables _ ht) = liftIO . stToIO $ HT.lookup ht idt

setGlobalVariable :: MonadIO m => Identifier -> WorldVariable -> Variables -> m ()
setGlobalVariable idt newVal (Variables ht _) = liftIO . stToIO $ HT.insert ht idt newVal

setWorldVariable :: MonadIO m => Identifier -> WorldVariable -> Variables -> m ()
setWorldVariable idt newVal (Variables _ ht) = liftIO . stToIO $ HT.insert ht idt newVal

replaceWorldVariables :: MonadIO m => Variables -> IDMap WorldVariable -> m (IDMap WorldVariable)
replaceWorldVariables variables !newVars = liftIO . stToIO $ do
    let worldVars = worldspaceVariables variables
    -- we convert the previous HashTable into just a normal Map
    prevWorldMap <- Map.fromList <$> HT.foldM (\acc cur -> pure (cur:acc)) [] worldVars

    HT.mapM_ (\(k, _) -> HT.delete worldVars k) worldVars

    forM_ (Map.assocs newVars) $ uncurry (HT.insert worldVars)

    return prevWorldMap
module Kathu.Entity.Components where

import Apecs
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types       (typeMismatch)
import Data.Text              (Text)
import qualified Data.Set     as DSet
import GHC.Generics

import Kathu.Entity.Action
import Kathu.Util.Types       (Identifier, mkIdentifier)

type CacheSize = 4096

-- Component types and instances

-- | A component that we enforce all created entities can hold without any differences
data Existance = Existance

instance Component Existance where type Storage Existance = Cache CacheSize (Map Existance)

-- | If all entities are created through this, then we can use it to get all entities
newExistingEntity :: (MonadIO m, Set w m c, Set w m Existance, Get w m EntityCounter) => c -> SystemT w m Entity
newExistingEntity c = newEntity (Existance, c)

-- | Mutually exclusive categories that mark special properties about a given entity
data SpecialEntity
    = WorldCollision
    | Spirit
    | EditorRefTo Entity
    deriving (Show, Eq)

instance Component SpecialEntity where type Storage SpecialEntity = Cache CacheSize (Map SpecialEntity)

-- Not all can be parsed, as some are while-running only
instance FromJSON SpecialEntity where
    parseJSON = withText "SpecialEntity" $ \case
        "spirit" -> pure Spirit
        e        -> fail $ "Unknown SpecialEntity " ++ show e

data Identity = Identity
    { identifier  :: Identifier
    , name        :: Text
    , description :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON Identity

simpleIdentity :: Identifier -> Identity
simpleIdentity idt = Identity idt "" ""

instance Component Identity where type Storage Identity = Cache CacheSize (Map Identity)

instance FromJSON Identity where
    parseJSON (String s) = pure $ Identity (mkIdentifier s) "" "" -- basic one with only an id
    parseJSON (Object v) = Identity <$> v .: "id" <*> v .:? "name" .!= "" <*> v .:? "description" .!= ""
    parseJSON e          = typeMismatch "Identity" e

newtype MovingSpeed = MovingSpeed Double deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Component MovingSpeed where type Storage MovingSpeed = Map MovingSpeed

newtype Tags = Tags (DSet.Set Text) deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Component Tags where type Storage Tags = Map Tags

-- Uniques

data Player = Player

instance Component Player where type Storage Player = Unique Player

newtype Local = Local {actionPressed :: ActionPressed}

instance Component Local where type Storage Local = Unique Local
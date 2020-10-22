module Kathu.Scripting.ExternalFunctions where

import           Apecs

import           Kathu.Entity.Prefab
import           Verda.Util.Types

-- | A collection of functions that are not currently possible to implement with just the library project, but are required for scripts to run
data ExternalFunctions w = ExternalFunctions
    { setPalette      :: Identifier -> SystemT w IO Bool -- Identifier to change to -> True if successful change
    , getEntityPrefab :: Identifier -> SystemT w IO (Maybe Prefab)
    , newFromPrefab   :: Prefab -> SystemT w IO Entity
    , destroyEntity   :: Entity -> SystemT w IO ()
    }
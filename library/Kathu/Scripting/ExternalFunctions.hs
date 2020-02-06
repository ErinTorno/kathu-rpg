module Kathu.Scripting.ExternalFunctions where

import           Apecs

import           Kathu.Entity.Prototype
import           Kathu.Util.Types

-- | A collection of functions that are not currently possible to implement with just the library project, but are required for scripts to run
data ExternalFunctions w g = ExternalFunctions
    { setPalette         :: Identifier -> SystemT w IO Bool -- Identifier to change to -> True if successful change
    , getEntityPrototype :: Identifier -> SystemT w IO (Maybe (EntityPrototype g))
    , newFromPrototype   :: EntityPrototype g -> SystemT w IO Entity
    , destroyEntity      :: Entity -> SystemT w IO ()
    }
module Kathu.Entity.Player where

import Apecs

-- Holds player specific information seperate from the ecs
data Player = Player
    { playerID :: Int
    , entityID :: Entity
    }
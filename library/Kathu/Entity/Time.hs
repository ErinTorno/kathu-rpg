module Kathu.Entity.Time where

import Data.Word

-- render and logic are kept separate, as the times for each type of loop may become desynced
-- ex: We may update the physics multiple times to "catch-up" if there was a delay in the system
--     Or update the graphics multiple times to display higher frames while we wait for the physics to run again
newtype LogicTime = LogicTime {unLogicTime :: Word32} deriving (Show, Eq)

newtype RenderTime = RenderTime {unRenderTime :: Word32} deriving (Show, Eq)
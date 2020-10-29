module Verda.Time where

import           Apecs
import           Control.Monad.IO.Class (MonadIO)
import           Data.Word

-- render and logic are kept separate, as the times for each type of loop may become desynced
-- ex: We may update the physics multiple times to "catch-up" if there was a delay in the system
--     Or update the graphics multiple times to display higher frames while we wait for the physics to run again
newtype LogicTime = LogicTime {unLogicTime :: Word32} deriving (Show, Eq)

newtype RenderTime = RenderTime {unRenderTime :: Word32} deriving (Show, Eq)

instance Semigroup LogicTime where (<>) = mappend
instance Monoid LogicTime where mempty  = LogicTime 0
instance Component LogicTime where type Storage LogicTime = Global LogicTime

instance Semigroup RenderTime where (<>) = mappend
instance Monoid RenderTime where mempty  = RenderTime 0
instance Component RenderTime where type Storage RenderTime = Global RenderTime

stepLogicTime :: forall w m. (Has w m LogicTime, MonadIO m) => Word32 -> SystemT w m ()
stepLogicTime !dT = modify global $ \(LogicTime t) -> LogicTime (t + dT)

stepRenderTime :: forall w m. (Has w m RenderTime, MonadIO m) => Word32 -> SystemT w m ()
stepRenderTime !dT = modify global $ \(RenderTime t) -> RenderTime (t + dT)
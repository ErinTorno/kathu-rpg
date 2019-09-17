{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Kathu.Game (runGame) where

import Apecs hiding (set)
import Apecs.System (cmapIf)
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe
import Data.Word
import Linear.V3 (V3(..))

import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Util.Apecs
import Kathu.World.Time (WorldTime)

runPhysics :: forall w m. (MonadIO m, HasEach w m '[ActionSet, Local, MovingSpeed, Position, Velocity]) => SystemT w m ()
runPhysics = do
    -- Updates ActionSet for the local player based on pressed 
    cmap $ \(as, Local press) ->
        let dir = getDirection press
            facingDir = fromMaybe (view facingDirection as) dir
        in set facingDirection facingDir . set moving dir . set lastMoving (view moving as) $ as
    -- Updates Velocity for all moving acting entities
    cmapIf newDirection $ \(as, MovingSpeed s, Velocity _) -> Velocity . fromMaybe (V3 0 0 0) . fmap (getMoveVector s) . view moving $ as
    -- Updates Position based on Velocity
    cmap $ \(Position p, Velocity v) -> Position (v + p)
    pure ()

runGame :: forall w m. (MonadIO m, HasEach w m '[ActionSet, Local, LogicTime, MovingSpeed, Position, RenderTime, Velocity, WorldTime])
        => Word32 -> SystemT w m ()
runGame !dT = do
    stepLogicTime dT
    stepWorldTime dT
    runPhysics
    pure ()

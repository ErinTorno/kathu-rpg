{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Kathu.Game (runGame, updateDelay) where

import Apecs hiding (set)
import Apecs.Physics hiding (set)
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe
import Data.Word

import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.LifeTime
import Kathu.Entity.Physics.Floor
import Kathu.Entity.System
import Kathu.Entity.Time
import Kathu.Util.Apecs
import Kathu.Util.Timing
import Kathu.World.Time (WorldTime)

updateDelay :: Word32
updateDelay = floor $ 1000 / (60 :: Double) -- 60 ticks per second is ideal

runPhysics :: forall w m. (MonadIO m, Get w m EntityCounter, Has w m Physics, ReadWriteEach w m '[ActionSet, Existance, FloorProperties, Local, MovingSpeed, WorldFloor])
           => SystemT w m ()
runPhysics = do
    (FloorProperties defFloorPropEty _) <- get global

    -- We assign the default floor to entities that have a WorldFloor that requests for it to be assigned
    cmapIfM isFloorUnassigned (assignWorldFloor defFloorPropEty)
    
    -- Updates ActionSet for the local player based on pressed 
    cmap $ \(as, Local press) ->
        let dir       = getDirection press
            facingDir = fromMaybe (as^.facingDirection) dir
         in set isFocused (timedVal $ press^.useFocus) . set facingDirection facingDir . set moving dir . set lastMoving (as^.moving) $ as
    -- Applies for all moving acting entities
    cmap $ \(MovingSpeed s, Velocity v, Mass m, as) -> Force
                                                     . getMoveVector v (movingAcceleration m s) (if as^.isFocused then s / 2 else s)
                                                     . view moving
                                                     $ as
    pure ()
    
runGame :: forall w m. (MonadIO m, Get w m EntityCounter, Has w m Physics, ReadWriteEach w m '[ActionSet, Existance, FloorProperties, LifeTime, Local, LogicTime, MovingSpeed, WorldFloor, WorldTime])
        => (Entity -> SystemT w m ()) -- We take a function to destroy an entity, since there are more components than this module knows about
        -> Word32
        -> SystemT w m ()
runGame destroyEntity !dT = do
    cmap   $ updateLifeTime dT
    cmapM_ $ \(life, ety) -> when (hasExpired life) (destroyEntity ety)
    stepLogicTime dT
    stepWorldTime dT
    runPhysics
    pure ()
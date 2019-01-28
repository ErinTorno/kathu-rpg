{-# LANGUAGE BangPatterns #-}

module Game where

import Apecs
import Apecs.System (cmapIf)
import Data.Maybe
import Data.Word
import qualified SDL

import Entity.Action
import Entity.Components
import Entity.System
import Entity.Timing
import qualified SDLCommon as SDLC

runPhysics :: System' ()
runPhysics = do
    --let updateCheck v = if v == SDL.V3 0.0 0.0 0.0 then v else error . show $ v
    -- Updates ActionSet for the local player based on pressed 
    cmap $ \(set@(ActionSet {}), Local press) -> let lastM = moving set in set {lastMoving = lastM, moving = getDirection press}
    -- Updates Velocity for all moving acting entities
    cmapIf newDirection $ \(ActionSet _ m _ _ _, MovingSpeed s, Velocity v) -> Velocity . fromMaybe SDLC.zeroV3 . fmap (getMoveVector s) $ m
    -- Updates Position based on Velocity
    cmap $ \(Position p, Velocity v) -> Position (v + p)
    pure ()

runGame :: Word32 -> System' ()
runGame !dT = do
    updateActions
    stepPhysicsTime dT
    runPhysics
    pure ()

-- System input

updateActions :: System' ()
updateActions = do
    (PhysicsTime time) <- get global
    ikp <- SDL.getKeyboardState
    cmap $ \(Local act, Position pos) -> if ikp SDL.ScancodeQ then error . show $ pos else Position pos
    -- don't updating timestamp if its just the same state again
    let updateTS (TimeStamped b t) nb = if b /= nb then TimeStamped nb time else TimeStamped b t
        updateKeys (Local actions@(ActionPressed {moveNorth = n, moveEast = e, moveSouth = s, moveWest = w})) = Local newActions
            where newActions = actions {moveNorth = ukp n SDL.ScancodeW, moveEast = ukp e SDL.ScancodeD, moveSouth = ukp s SDL.ScancodeS, moveWest = ukp w SDL.ScancodeA}
                  ukp ts = updateTS ts . ikp
    cmap updateKeys
{-# LANGUAGE BangPatterns #-}

module Kathu.Game where

import Apecs hiding (set)
import Apecs.System (cmapIf)
import Control.Lens
import Data.Maybe
import Data.Word
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import qualified Kathu.Util.SDLCommon as SDLC
import Kathu.Util.Timing
import qualified SDL

runPhysics :: System' ()
runPhysics = do
    -- Updates ActionSet for the local player based on pressed 
    cmap $ \(as, Local press) ->
        let dir = getDirection press
            facingDir = fromMaybe (view facingDirection as) dir
        in set facingDirection facingDir . set moving dir . set lastMoving (view moving as) $ as
    -- Updates Velocity for all moving acting entities
    cmapIf newDirection $ \(as, MovingSpeed s, Velocity v) -> Velocity . fromMaybe SDLC.zeroV3 . fmap (getMoveVector s) . view moving $ as
    -- Updates Position based on Velocity
    cmap $ \(Position p, Velocity v) -> Position (v + p)
    pure ()

runGame :: Word32 -> System' ()
runGame !dT = do
    updateActions
    stepLogicTime dT
    runPhysics
    pure ()

-- System input

updateActions :: System' ()
updateActions = do
    (LogicTime time) <- get global
    ikp <- SDL.getKeyboardState
    -- don't updating timestamp if its just the same state again
    let updateTS nb ts@(TimeStamped b t) = if b /= nb then TimeStamped nb time else ts
        updateKeys (Local actions) = Local (mkNewActions actions)
            where ukp key getter = over getter (updateTS $ ikp key)
                  mkNewActions = ukp SDL.ScancodeA moveWest . ukp SDL.ScancodeD moveEast . ukp SDL.ScancodeW moveNorth . ukp SDL.ScancodeS moveSouth
    cmap updateKeys
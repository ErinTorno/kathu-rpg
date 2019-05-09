{-# LANGUAGE BangPatterns #-}

module Kathu.Game where

import Apecs hiding (set)
import Apecs.System (cmapIf)
import Control.Lens
import Control.Monad (when)
import Data.Maybe
import Data.Word
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Graphics.Camera
import Kathu.Graphics.ImageManager (setPalette, currentPalette, availablePaletteCount)
import Kathu.IO.Settings
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
    settings         <- get global
    let cs = controls settings

    ikp <- SDL.getKeyboardState

    -- checks if we want to toggle debug mode or not
    when (canUseDebug settings && ikp (keyToggleDebug cs)) $ do
        (Debug d) <- get global
        global $= Debug (not d)

    Debug isDebug <- get global
    when isDebug $ do
        -- controls zooming of the camera
        cmap (\(Camera z) -> if ikp (keyDebugZoomIn cs) then Camera (z + 0.05) else if ikp (keyDebugZoomOut cs) then Camera (z - 0.05) else Camera z)
        -- moves to next theme
        when (ikp (keyDebugNextPalette cs)) $ do
            im <- get global
            let nextPalette = (1 + currentPalette im) `mod` (availablePaletteCount im)
            global $= setPalette nextPalette im

    -- don't updating timestamp if its just the same state again
    let updateTS nb ts@(TimeStamped b t) = if b /= nb then TimeStamped nb time else ts
        updateKeys (Local actions) = Local (mkNewActions actions)
            where ukp key getter = over getter (updateTS $ ikp key)
                  mkNewActions = ukp (keyMoveWest cs) moveWest . ukp (keyMoveEast cs) moveEast . ukp (keyMoveNorth cs) moveNorth . ukp (keyMoveSouth cs) moveSouth
    cmap updateKeys
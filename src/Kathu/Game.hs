{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Kathu.Game (runGame, runEvents) where

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
import Kathu.Util.Misc (whileFstM)
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
    cs  <- controls <$> get global
    ikp <- SDL.getKeyboardState

    -- don't updating timestamp if its just the same state again
    let updateTS nb ts@(TimeStamped b t) = if b /= nb then TimeStamped nb time else ts
        updateKeys (Local actions) = Local (mkNewActions actions)
            where ukp key getter = over getter (updateTS $ ikp key)
                  mkNewActions = ukp (keyMoveWest cs) moveWest . ukp (keyMoveEast cs) moveEast . ukp (keyMoveNorth cs) moveNorth . ukp (keyMoveSouth cs) moveSouth
    cmap updateKeys

runEvents :: SystemT' IO Bool
runEvents = whileFstM (SDL.pollEvent >>= ev)
    where -- stop running, with end result of False
          ev (Just (SDL.Event _ SDL.QuitEvent)) = pure (False, False)
          -- key is pressed, but not repeating
          ev (Just (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False keysym)))) = do
              settings <- get global
              Debug isDebug <- get global
              let cs  = controls settings
                  key = SDL.keysymScancode keysym
                
              if canUseDebug settings && key == keyToggleDebug cs then do
                  (Debug d) <- get global
                  global $= Debug (not d)
              else if isDebug then
                  -- debug controls are kept here, to prevent toggling too fast whenever a frame is processed
                  if key == keyDebugZoomIn cs then do
                      cmap $ \(Camera z) -> Camera (z + 0.5)
                  else if key == keyDebugZoomOut cs then do
                      cmap $ \(Camera z) -> Camera (z - 0.5)
                  else if key == keyDebugNextPalette cs then do
                      im <- get global
                      let nextPalette = (1 + currentPalette im) `mod` (availablePaletteCount im)
                      global $= setPalette nextPalette im
                   else pure ()
              else pure ()
              pure (True, True)
          ev (Just _) = pure (True, True)
          -- stop running, with end result of True
          ev Nothing  = pure (False, True)
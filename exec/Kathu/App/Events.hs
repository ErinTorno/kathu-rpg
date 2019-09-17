{-# LANGUAGE FlexibleContexts #-}

module Kathu.App.Events where

import Apecs hiding (set)
import Control.Lens
import qualified SDL

import Kathu.App.Data.Settings
import Kathu.App.Graphics.ImageManager (setPalette, currentPalette, availablePaletteCount)
import Kathu.App.System
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Graphics.Camera
import Kathu.Util.Flow (whileFstM)
import Kathu.Util.Timing

handleControls :: System' ()
handleControls = do
    (LogicTime time) <- get global
    cs  <- controls <$> get global
    ikp <- SDL.getKeyboardState

    -- don't updating timestamp if its just the same state again
    let updateTS nb ts@(TimeStamped b _) = if b /= nb then TimeStamped nb time else ts
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
              settings      <- get global
              Debug isDebug <- get global
              let cs         = controls settings
                  key        = SDL.keysymScancode keysym
                
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
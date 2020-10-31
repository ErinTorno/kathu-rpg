module Verda.Event.EventHandler
    ( handleEvents
    ) where

import           Apecs
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Maybe                (fromMaybe)
import           Linear.V2
import qualified SDL

import           Verda.Event.Controls
import           Verda.Graphics.Components (Camera(..), unResolution)
import           Verda.Graphics.Drawing
import           Verda.World
import           Verda.Util.Apecs

handleEvents :: (MonadIO m, VerdaWorld w m) => SystemT w m ()
handleEvents = do
    controlSt       <- get global
    cursorMotion    <- get global
    res@(V2 _ resY) <- unResolution <$> get global

    nextInputStateFrame controlSt
    -- set motion to zero now
    global       $= cursorMotion {cursorMovement = V2 0 0, cursorScrollWheel = 0}

    (Position (V2 camX camY), Camera zoomScale) <- fromMaybe (Position (V2 0 0), Camera 1) <$> getUnique

    let unitsPerHeight = getUnitsPerHeight resY
        scale          = getScale (fromIntegral resY) unitsPerHeight zoomScale
        screenToWorld  = screenToWorldScale (fromIntegral <$> res) scale camX camY 

    SDL.mapEvents $ handleEvent controlSt scale
    -- sets the cursor position to a world-adjusted coordinate from the current pixel it is at
    cursorMotion' <- get global
    global $= cursorMotion' {cursorPosition = screenToWorld . fmap fromIntegral $ cursorScreenPosition cursorMotion'}

handleEvent :: (MonadIO m, VerdaWorld w m) => ControlState -> Double -> SDL.Event -> SystemT w m ()
handleEvent controlSt scale event =
    case SDL.eventPayload event of
        SDL.QuitEvent -> global $= Quitting
        -- marks key with its state
        SDL.KeyboardEvent (SDL.KeyboardEventData _ motion _ keysym) ->
            let key = fromScanCode $ SDL.keysymScancode keysym
            in updateInputCode controlSt key motion
        -- sets mouse clicks
        SDL.MouseButtonEvent SDL.MouseButtonEventData{SDL.mouseButtonEventMotion = motion, SDL.mouseButtonEventButton = btn} ->
            updateInputCode controlSt (fromMouseButton btn) motion
        -- sets mouse's position and motion
        -- we want to convert this from screen pixels to game world units
        SDL.MouseMotionEvent SDL.MouseMotionEventData{SDL.mouseMotionEventPos = SDL.P pos, SDL.mouseMotionEventRelMotion = relPos} -> do
            motionState <- get global
            global      $= motionState { cursorScreenPosition = pos
                                       , cursorMovement       = (/(scale * pixelsPerUnit)) . fromIntegral <$> relPos}
        -- sets mouse's scrolling amount
        SDL.MouseWheelEvent SDL.MouseWheelEventData{SDL.mouseWheelEventPos = V2 _ y, SDL.mouseWheelEventDirection = dir} -> do
            motionState <- get global
            global      $= motionState {cursorScrollWheel = fromIntegral y * (if dir == SDL.ScrollFlipped then -1 else 1)}
        _ -> pure ()
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.App.Events where

import Apecs hiding (set)
import Apecs.Physics
import Control.Lens hiding (Identity)
import Control.Monad (void, when)
import qualified SDL

import Kathu.App.Data.Settings
import Kathu.App.Graphics.ImageManager (nextPaletteManager, setPaletteManager)
import Kathu.App.System
import Kathu.Entity.Action
import Kathu.Entity.Components
import Kathu.Entity.System
import Kathu.Entity.Time
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
                  mkNewActions = ukp (keyMoveWest cs) moveWest
                               . ukp (keyMoveEast cs) moveEast
                               . ukp (keyMoveNorth cs) moveNorth
                               . ukp (keyMoveSouth cs) moveSouth
                               . ukp (keyFocus cs) useFocus
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
              else when isDebug $
                  -- debug controls are kept here, to prevent toggling too fast whenever a frame is processed
                  if      key == keyDebugZoomIn cs       then cmap $ \(Camera z) -> Camera (z + 0.25)
                  else if key == keyDebugZoomOut cs      then cmap $ \(Camera z) -> Camera (z - 0.25)
                  else if key == keyDebugPrintPhysics cs then printPhysics
                  else if key == keyDebugNextPalette cs  then do
                      im <- get global
                      void $ setPaletteManager (nextPaletteManager im)
                  else pure ()
              pure (True, True)
          ev (Just _) = pure (True, True)
          -- stop running, with end result of True
          ev Nothing  = pure (False, True)

printPhysics :: SystemT' IO ()
printPhysics = do
    liftIO . putStrLn $ "### Print Physics ###"
    
    (Gravity g)    <- get global
    (Iterations i) <- get global
    liftIO . putStrLn . concat $ ["global (gravity ", show g, "; iterations ", show i, ")"]

    -- Torque and force are ignored, as those are set to zero every update, and so this would always print 0's for them
    cmapM_ $ \(Identity idt _ _, (Position p, Velocity v), (BodyMass mass, Moment moment), (Angle a, AngularVelocity a', CenterOfGravity cog), Entity etyID :: Entity) ->
        liftIO . putStrLn . concat $
            [ show etyID
            , " (", show idt
            , "): pos ", show p
            , "; vel ", show v
            , "; mass ", show mass
            , "; moment ", show moment
            , "; angle ", show a
            , "; angle-v ", show a'
            , "; center-of-g ", show cog
            , ")"
            ]
    liftIO . putStrLn $ "### End of Print Physics ###"
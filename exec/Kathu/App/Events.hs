module Kathu.App.Events (runEvents) where

import           Apecs
import           Apecs.Physics
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (void, when)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text.IO                    as T
import qualified SDL
import           Verda.Event.Controls
import           Verda.Util.Apecs
import           Verda.Util.Types

import           Kathu.App.Data.Controls
import           Kathu.App.Data.Settings
import           Kathu.App.Graphics.Drawing
import           Kathu.App.Graphics.ImageManager (nextPaletteManager, setPaletteManager)
import           Kathu.App.System
import           Kathu.Entity.Action
import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Entity.Time
import           Kathu.Graphics.Camera

runEvents :: System' ()
runEvents = do
    controlSt <- get global
    nextInputStateFrame controlSt

    -- set motion to zero now
    cursorMotion <- get global
    global       $= cursorMotion {cursorMovement = V2 0 0, cursorScrollWheel = 0}

    settings  <- get global
    (Position (V2 camX camY), Camera zoomScale) <- fromMaybe (Position (V2 0 0), Camera 1) <$> getUnique

    let V2 _ resY      = resolution settings
        unitsPerHeight = getUnitsPerHeight resY
        scale          = getScale (fromIntegral resY) unitsPerHeight zoomScale
        screenToWorld  = screenToWorldScale (fromIntegral <$> resolution settings) scale camX camY 

    SDL.mapEvents (handleEvent controlSt scale)
    cursorMotion' <- get global
    -- sets the cursor position to a world-adjusted coordinate from the current pixel it is at
    global        $= cursorMotion' {cursorPosition = screenToWorld . fmap fromIntegral $ cursorScreenPosition cursorMotion'}

    updateControls
    updateDebugControls

handleEvent :: ControlState -> Double -> SDL.Event -> SystemT' IO ()
handleEvent controlSt scale event =
    case SDL.eventPayload event of
        SDL.QuitEvent -> global $= ShouldQuit True
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

updateControls :: SystemT' IO ()
updateControls = do
    controlSt      <- get global
    cs             <- controls <$> get global
    LogicTime time <- get global

    let -- don't update the timestamp unless the state has changed
        updateTimeStamp nb ts@(TimeStamped b _) = if b /= nb then TimeStamped nb time else ts
        ukp inputCode getter actions = do
            st <- getInputState controlSt inputCode
            pure $ over getter (updateTimeStamp (isPressedOrHeld st)) actions
        -- updates all actions according to their control in the settings
        updateActions actions = ukp (inputMoveWest cs) moveWest actions
                            >>= ukp (inputMoveEast cs) moveEast
                            >>= ukp (inputMoveNorth cs) moveNorth
                            >>= ukp (inputMoveSouth cs) moveSouth
                            >>= ukp (inputFocus cs) useFocus
    cmapM $ \(Local actions) -> Local <$> updateActions actions

------------------
-- Debug Events --
------------------

updateDebugControls :: SystemT' IO ()
updateDebugControls = do
    controlSt         <- get global
    settings          <- get global
    when (canUseDebug settings) $ do
        let cs     = controls settings

        debugKeySt <- getInputState controlSt (inputToggleDebug cs)
        when (debugKeySt == BtnPressed) $ do
            Debug isDebug <- get global
            global        $= Debug (not isDebug) -- toggle debug

        Debug isDebug <- get global
        when isDebug $ do
            let whenPressed code action = getInputState controlSt code >>= \st -> when (st == BtnPressed) action

            whenPressed (inputDebugZoomIn cs) $
                cmap $ \(Camera z) -> Camera (z + 0.25)
            whenPressed (inputDebugZoomOut cs) $
                cmap $ \(Camera z) -> Camera (z - 0.25)

            whenPressed (inputDebugPrintPhysics cs)
                printPhysics
            whenPressed (inputDebugNextPalette cs) $ do
                im <- get global
                void $ setPaletteManager (nextPaletteManager im)

printPhysics :: SystemT' IO ()
printPhysics = do
    liftIO . putStrLn $ "### Print Physics ###"
    
    (Gravity g)    <- get global
    (Iterations i) <- get global
    liftIO . putStrLn . concat $ ["global (gravity ", show g, "; iterations ", show i, ")"]

    let showColFilter Nothing  = "Nothing"
        showColFilter (Just (CollisionFilter group catMask filMask)) = concat ["(group ", show group, "; category ", show catMask, "; filter ", show filMask, ")"]

    -- Torque and force are ignored, as those are set to zero every update, and so this would always print 0's for them
    cmapM_ $ \(Identity idt _ _, (Position p, Velocity v, BodyMass mass), (Angle a, AngularVelocity a', CenterOfGravity cog), Entity etyID :: Entity, col :: Maybe CollisionFilter) -> liftIO $ do
        putStr . show $ etyID
        putStr " ("
        T.putStr . unID $ idt -- this is separate to prevent unicode encoding issues with show
        putStr ")"
        putStrLn . concat $
            [ ": pos ", show p
            , "; vel ", show v
            , "; mass ", show mass
            , "; angle ", show a
            , "; angle-v ", show a'
            , "; center-of-g ", show cog
            , "; col-filter ", showColFilter col
            , ")"
            ]
    liftIO . putStrLn $ "### End of Print Physics ###"
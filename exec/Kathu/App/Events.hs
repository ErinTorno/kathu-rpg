module Kathu.App.Events (runEvents) where

import           Apecs
import           Apecs.Physics
import           Control.Lens                    hiding (Identity)
import           Control.Monad                   (void, when)
import qualified Data.Text.IO                    as T
import           Verda.Event.Controls
import           Verda.Event.EventHandler        (handleEvents)
import           Verda.Graphics.Components       (Camera(..))
import           Verda.Graphics.SpriteManager    (nextPaletteManager, setPaletteManager)
import           Verda.Time
import           Verda.Util.Types

import           Kathu.App.Data.Controls
import           Kathu.App.Data.Settings
import           Kathu.App.System
import           Kathu.Entity.Action
import           Kathu.Entity.Components
import           Kathu.Entity.System

runEvents :: System' ()
runEvents = do
    handleEvents
    updateControls
    updateDebugControls

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
                mgr <- get global
                void $ setPaletteManager (nextPaletteManager mgr)

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
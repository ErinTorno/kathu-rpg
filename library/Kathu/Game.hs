module Kathu.Game (initPhysics, runGame, updateDelay) where

import           Apecs                               hiding (set)
import           Apecs.Physics                       hiding (set)
import           Control.Lens
import           Control.Monad                       (forM_, when)
import           Data.Maybe
import           Data.Word
import           Verda.Util.Apecs
import           Verda.Util.Types
import           Verda.Time                          (stepLogicTime)

import           Kathu.Entity.Action
import           Kathu.Entity.Components
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.CollisionGroup
import           Kathu.Entity.Physics.Floor
import           Kathu.Entity.System
import           Kathu.Entity.Utils                  (destroyEntity)
import           Kathu.Scripting.Event
import qualified Kathu.Scripting.Lua                 as Lua
import           Kathu.World.Time                    (stepWorldTime)

updateDelay :: Word32
updateDelay = floor $ 1000 / (60 :: Double) -- 60 ticks per second is ideal

initPhysics :: SystemT' IO ()
initPhysics = do
    let callSensorCollide event fnName colFilA etyA etyB =
            when (colFilA == movementSensorFilter) $ do
                maybeScript <- get etyA
                case maybeScript of
                    Just script -> when (Lua.shouldScriptRun event script) $ Lua.execFor script (Lua.call fnName (unEntity etyA) (unEntity etyB))
                    Nothing     -> pure ()

    begin <- mkBeginCB $ \(Collision _ bodyA bodyB shapeA _) -> do
        colFilA <- get shapeA
        -- only check for A as sensor, since the collision event will get called a second time with flipped entities
        callSensorCollide onSensorCollisionBegin "onSensorCollisionBegin" colFilA bodyA bodyB
        pure True

    separate <- mkSeparateCB $ \(Collision _ bodyA bodyB shapeA _) -> do
        maybeColFilA :: Maybe CollisionFilter <- get shapeA
        -- need to maybe check since entity might be deleted mid-collision
        forM_ maybeColFilA $ \colFilA ->
            callSensorCollide onSensorCollisionEnd "onSensorCollisionEnd" colFilA bodyA bodyB

    global $= defaultHandler
        { beginCB    = Just begin
        , separateCB = Just separate
        }

runPhysics :: SystemT' IO ()
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
    
runGame :: Word32 -> SystemT' IO ()
runGame !dT = do
    cmap   $ updateLifeTime dT
    cmapM_ $ \(life, ety) -> when (hasExpired life) (destroyEntity ety)
    stepLogicTime dT
    stepWorldTime dT

    cmapIfM (Lua.shouldScriptRun onUpdate) $ \(activeScript, Entity ety) ->
        Lua.execFor activeScript (Lua.call "onUpdate" ety)

    runPhysics
    pure ()
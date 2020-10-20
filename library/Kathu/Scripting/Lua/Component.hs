module Kathu.Scripting.Lua.Component (registerComponentFunctions) where

import           Apecs
import           Apecs.Physics
import           Control.Monad                       (forM_, when)
import qualified Data.Map                            as Map
import           Data.Maybe                          (fromJust, isJust, isNothing)
import           Data.Text                           (Text)
import qualified Data.Vector                         as Vec
import qualified Data.Set                            as DSet
import qualified Data.Text                           as T
import           Foreign.Lua

import           Kathu.Entity.Components
import           Kathu.Entity.Logger
import           Kathu.Entity.Physics.CollisionGroup
import           Kathu.Graphics.Drawable             (Render(..), RenderSprite(..), switchAnimationByID)
import           Kathu.Scripting.ExternalFunctions
import           Kathu.Scripting.Lua.Types
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Verda.Util.Types
import           Verda.Util.Apecs

-- ExternalFunctions is unused in this, but is included here since it might be in the future, mirrors the Global's function signature, and acts as a Proxy for g
registerComponentFunctions :: forall w g. (Has w IO Physics, Members w IO (Render g), ReadWriteEach w IO '[ActiveScript, Force, Identity, Logger, Mass, MovingSpeed, Position, Render g, RunningScriptEntity, ScriptEventBuffer, Tags, Velocity, WireReceivers])
                           => w -> ExternalFunctions w g -> Lua ()
registerComponentFunctions world _ = do
    registerHaskellFunction "getIdentifier"        $ getIdentifier world
    registerHaskellFunction "getName"              $ getName world
    registerHaskellFunction "getDescription"       $ getDescription world
    registerHaskellFunction "getTags"              $ getTags world
    registerHaskellFunction "getMovingSpeed"       $ getMovingSpeed world
    registerHaskellFunction "setMovingSpeed"       $ setMovingSpeed world
    registerHaskellFunction "setAnimation"         $ setAnimation (Proxy :: Proxy g) world
    registerHaskellFunction "getMass"              $ getMass world
    registerHaskellFunction "setMass"              $ setMass world
    registerHaskellFunction "getPosition"          $ getPosition world
    registerHaskellFunction "setPosition"          $ setPosition world
    registerHaskellFunction "getVelocity"          $ getVelocity world
    registerHaskellFunction "setVelocity"          $ setVelocity world
    registerHaskellFunction "getForce"             $ getForce world
    registerHaskellFunction "setForce"             $ setForce world
    registerHaskellFunction "setCollisionCategory" $ setCollisionCategory world
    registerHaskellFunction "modifyWirePower"      $ modifyWirePower world
    registerHaskellFunction "getConfig"            $ getInstanceConfigVariable world

getIdentifier :: forall w. (ReadWrite w IO Identity) => w -> Int -> Lua (Optional Text)
getIdentifier !world !etyID = liftIO . Apecs.runWith world $ do
    idtc <- getIfExists (Entity etyID)
    pure $ case idtc of
        Just idt -> Optional $ Just (unID . identifier $ idt)
        Nothing  -> Optional Nothing

getName :: forall w. (ReadWrite w IO Identity) => w -> Int -> Lua (Optional Text)
getName !world !etyID = liftIO . Apecs.runWith world $ do
    idtc <- getIfExists (Entity etyID)
    pure $ case idtc of
        Just idt -> Optional $ Just (name idt)
        Nothing  -> Optional Nothing

getDescription :: forall w. (ReadWrite w IO Identity) => w -> Int -> Lua (Optional Text)
getDescription !world !etyID = liftIO . Apecs.runWith world $ do
    idtc <- getIfExists (Entity etyID)
    pure $ case idtc of
        Just idt -> Optional $ Just (description idt)
        Nothing  -> Optional Nothing

getTags :: forall w. (ReadWrite w IO Tags) => w -> Int -> Lua (Optional [Text])
getTags !world !etyID = liftIO . Apecs.runWith world $ do
    tags <- getIfExists (Entity etyID)
    pure $ case tags of
        Just (Tags t) -> Optional $ Just (DSet.toList t)
        Nothing       -> Optional Nothing

getMovingSpeed :: forall w. (ReadWrite w IO MovingSpeed) => w -> Int -> Lua (Optional Double)
getMovingSpeed !world !etyID = liftIO . Apecs.runWith world $ do
    mspeed <- getIfExists (Entity etyID)
    pure $ case mspeed of
        Just (MovingSpeed s) -> Optional $ Just s
        Nothing              -> Optional Nothing

setMovingSpeed :: forall w. (ReadWrite w IO MovingSpeed) => w -> Int -> Double -> Lua ()
setMovingSpeed !world !etyID s = liftIO . Apecs.runWith world $
    Entity etyID $= MovingSpeed s

--------------
-- Graphics --
--------------

setAnimation :: forall w g. (Members w IO (Render g), ReadWrite w IO (Render g)) => Proxy g -> w -> Int -> Text -> Lua ()
setAnimation _ !world !etyID !animID = liftIO . Apecs.runWith world $ do
    let ety = Entity etyID
        changeAnim (RSAnimated anim) = RSAnimated $ switchAnimationByID (mkIdentifier animID) anim
        changeAnim e                 = e

    mrender :: Maybe (Render g) <- getIfExists ety
    case mrender of
        Nothing     -> return ()
        Just (Render layers) -> ety $= Render (changeAnim <$> layers)

setCollisionCategory :: forall w. (Has w IO Physics, ReadWriteEach w IO [CollisionFilter, Tags]) => w -> Entity -> Text -> Optional Text -> Lua ()
setCollisionCategory !world !ety colCategory (Optional tag) = liftIO . Apecs.runWith world $ do
    ShapeList shapes <- get ety
    
    let maybeColGroup = collisionGroupFromText colCategory
    case groupCollisionFilter <$> maybeColGroup of
        Nothing        -> lift . putStrLn $ "Couldn't find collision category " ++ show colCategory
        Just colFilter ->
            forM_ shapes $ \sEty -> do
                maybeTags <- get sEty
                let colGroup = fromJust maybeColGroup
                case maybeTags of
                    Nothing          ->
                        sEty $= (colFilter, mkGroupSensor colGroup)
                    Just (Tags tags) ->
                        when (isJust tag && DSet.member (fromJust tag) tags) $
                            sEty $= (colFilter, mkGroupSensor colGroup)

-------------
-- Physics --
-------------

getVector2D :: forall w c. (Get w IO c, Members w IO c) => (c -> V2 Double) -> w -> Int -> Lua (Optional (V2 Double))
getVector2D mapper !world !etyID = liftIO . Apecs.runWith world $ do
    comp <- getIfExists (Entity etyID)
    pure $ case comp of
        Just vec  -> Optional . Just . mapper $ vec
        Nothing   -> Optional Nothing

getMass :: forall w. (ReadWrite w IO Mass) => w -> Int -> Lua (Optional Double)
getMass !world !etyID = liftIO . Apecs.runWith world $ do
    mass <- getIfExists (Entity etyID)
    pure $ case mass of
        Just (Mass m) -> Optional $ Just m
        Nothing       -> Optional Nothing

setMass :: forall w. (Has w IO Physics) => w -> Int -> Double-> Lua ()
setMass !world !etyID m = liftIO . Apecs.runWith world $
    Entity etyID $= Mass m

getPosition :: forall w. (ReadWrite w IO Position) => w -> Int -> Lua (Optional (V2 Double))
getPosition = getVector2D $ \(Position v) -> v

setPosition :: forall w. (ReadWrite w IO Position) => w -> Int -> V2 Double -> Lua ()
setPosition !world !etyID v = liftIO . Apecs.runWith world $
    Entity etyID $= Position v

getVelocity :: forall w. (ReadWrite w IO Velocity) => w -> Int -> Lua (Optional (V2 Double))
getVelocity = getVector2D $ \(Velocity v) -> v

setVelocity :: forall w. (ReadWrite w IO Velocity) => w -> Int -> V2 Double -> Lua ()
setVelocity !world !etyID v = liftIO . Apecs.runWith world $
    Entity etyID $= Velocity v

getForce :: forall w. (ReadWrite w IO Force) => w -> Int -> Lua (Optional (V2 Double))
getForce = getVector2D $ \(Force v) -> v

setForce :: forall w. (ReadWrite w IO Force) => w -> Int -> V2 Double -> Lua ()
setForce !world !etyID v = liftIO . Apecs.runWith world $
    Entity etyID $= Force v

-- Wires --

modifyWirePower :: forall w. (ReadWriteEach w IO [ActiveScript, RunningScriptEntity, ScriptEventBuffer, WireReceivers]) => w -> Int -> Int -> Lua ()
modifyWirePower !world !etyID !dPower = liftIO . Apecs.runWith world $ do
    receivers <- get global
    maybeScript :: Maybe ActiveScript <- get (Entity etyID)

    forM_ maybeScript $ \script -> Vec.forM_ (wireSignals script) $ \signalID ->
        mutateWirePower signalID (+dPower) receivers

-- Script --

getInstanceConfigVariable :: forall w. (ReadWriteEach w IO [ActiveScript, Logger, RunningScriptEntity]) => w -> Identifier -> Lua (Optional WorldVariable)
getInstanceConfigVariable !world !idt = liftIO . Apecs.runWith world $ do
    maybeEty <- runningScript <$> get global
    case maybeEty of
        Nothing  -> pure $ Optional Nothing
        Just ety -> do
            script    <- get ety
            let config = instanceConfig script
                var    = Map.lookup idt config
            when (isNothing var) $ 
                logLine Warning $ T.concat ["Config variable ", T.pack (show idt),  " is not present in the instance config for entity ", T.pack (show ety), " with config ", T.pack (show config), ", was this function called during the initial script parsing?"]
            pure . Optional $ var
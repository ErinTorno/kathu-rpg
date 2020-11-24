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
import           Verda.Graphics.Sprites              (Sprite, setAnimation)
import           Verda.Logger

import           Kathu.Entity.Components
import           Kathu.Entity.LifeTime
import           Kathu.Entity.Physics.CollisionGroup
import           Kathu.Entity.System                 (KathuWorld)
import           Kathu.Scripting.Lua.Types
import           Kathu.Scripting.Variables
import           Kathu.Scripting.Wire
import           Verda.Util.Types
import           Verda.Util.Apecs

registerComponentFunctions :: KathuWorld -> Lua ()
registerComponentFunctions world = do
    registerHaskellFunction "getIdentifier"        $ getIdentifier world
    registerHaskellFunction "getName"              $ getName world
    registerHaskellFunction "getDescription"       $ getDescription world
    registerHaskellFunction "getTags"              $ getTags world
    registerHaskellFunction "getMovingSpeed"       $ getMovingSpeed world
    registerHaskellFunction "setMovingSpeed"       $ setMovingSpeed world
    registerHaskellFunction "destroyEntity"        $ destroyEntity world
    registerHaskellFunction "setAnimation"         $ setSpriteAnimation world
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

getIdentifier :: KathuWorld -> Entity -> Lua (Optional Text)
getIdentifier !world !ety = liftIO . Apecs.runWith world $ do
    idtc <- getIfExists ety
    pure $ case idtc of
        Just idt -> Optional $ Just (unID . identifier $ idt)
        Nothing  -> Optional Nothing

getName :: KathuWorld -> Entity -> Lua (Optional Text)
getName !world !ety = liftIO . Apecs.runWith world $ do
    idtc <- getIfExists ety
    pure $ case idtc of
        Just idt -> Optional $ Just (name idt)
        Nothing  -> Optional Nothing

getDescription :: KathuWorld -> Entity -> Lua (Optional Text)
getDescription !world !ety = liftIO . Apecs.runWith world $ do
    idtc <- getIfExists ety
    pure $ case idtc of
        Just idt -> Optional $ Just (description idt)
        Nothing  -> Optional Nothing

getTags :: KathuWorld -> Entity -> Lua (Optional [Text])
getTags !world !ety = liftIO . Apecs.runWith world $ do
    tags <- getIfExists ety
    pure $ case tags of
        Just (Tags t) -> Optional $ Just (DSet.toList t)
        Nothing       -> Optional Nothing

getMovingSpeed :: KathuWorld -> Entity -> Lua (Optional Double)
getMovingSpeed !world !ety = liftIO . Apecs.runWith world $ do
    mspeed <- getIfExists ety
    pure $ case mspeed of
        Just (MovingSpeed s) -> Optional $ Just s
        Nothing              -> Optional Nothing

setMovingSpeed :: KathuWorld -> Entity -> Double -> Lua ()
setMovingSpeed !world !ety s = liftIO . Apecs.runWith world $
    ety $= MovingSpeed s

destroyEntity :: KathuWorld -> Entity -> Lua ()
destroyEntity !world !ety = liftIO . Apecs.runWith world $ do
    let zeroLT = ety $= LifeTimeTimer 0 -- will be removed at next update loop
    maybeLT :: Maybe LifeTime <- get ety
    case maybeLT of
        Nothing -> zeroLT
        Just lt -> when (lt /= Persistant) zeroLT

--------------
-- Graphics --
--------------

setSpriteAnimation :: KathuWorld -> Entity -> Identifier -> Lua ()
setSpriteAnimation !world !ety !animIdt = liftIO . Apecs.runWith world $ do
    mrender :: Maybe Sprite <- getIfExists ety
    case mrender of
        Nothing     -> pure ()
        Just sprite -> ety $= setAnimation animIdt sprite

setCollisionCategory :: KathuWorld -> Entity -> Text -> Optional Text -> Lua ()
setCollisionCategory !world !ety colCategory (Optional tag) = liftIO . Apecs.runWith world $ do
    let maybeColGroup = collisionGroupFromText colCategory
    case groupCollisionFilter <$> maybeColGroup of
        Nothing        -> lift . putStrLn $ "Couldn't find collision category " ++ show colCategory
        Just colFilter -> do
            ShapeList shapes <- get ety
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

getVector2D :: forall w c. Get w IO c => (c -> V2 Double) -> w -> Entity -> Lua (Optional (V2 Double))
getVector2D mapper !world !ety = liftIO . Apecs.runWith world $ do
    comp <- getIfExists ety
    pure $ case comp of
        Just vec  -> Optional . Just . mapper $ vec
        Nothing   -> Optional Nothing

getMass :: KathuWorld -> Entity -> Lua (Optional Double)
getMass !world !ety = liftIO . Apecs.runWith world $ do
    mass <- getIfExists ety
    pure $ case mass of
        Just (Mass m) -> Optional $ Just m
        Nothing       -> Optional Nothing

setMass :: KathuWorld -> Entity -> Double-> Lua ()
setMass !world !ety m = liftIO . Apecs.runWith world $
    ety $= Mass m

getPosition :: KathuWorld -> Entity -> Lua (Optional (V2 Double))
getPosition = getVector2D $ \(Position v) -> v

setPosition :: KathuWorld -> Entity -> V2 Double -> Lua ()
setPosition !world !ety v = liftIO . Apecs.runWith world $
    ety $= Position v

getVelocity :: KathuWorld -> Entity -> Lua (Optional (V2 Double))
getVelocity = getVector2D $ \(Velocity v) -> v

setVelocity :: KathuWorld -> Entity -> V2 Double -> Lua ()
setVelocity !world !ety v = liftIO . Apecs.runWith world $
    ety $= Velocity v

getForce :: KathuWorld -> Entity -> Lua (Optional (V2 Double))
getForce = getVector2D $ \(Force v) -> v

setForce :: KathuWorld -> Entity -> V2 Double -> Lua ()
setForce !world !ety v = liftIO . Apecs.runWith world $
    ety $= Force v

-- Wires --

modifyWirePower :: KathuWorld -> Entity -> Int -> Lua ()
modifyWirePower !world !ety !dPower = liftIO . Apecs.runWith world $ do
    receivers <- get global
    maybeScript :: Maybe ActiveScript <- get ety

    forM_ maybeScript $ \script -> Vec.forM_ (wireControllers script) $ \signalID ->
        mutateWirePower signalID (+dPower) receivers

-- Script --

getInstanceConfigVariable :: KathuWorld -> Identifier -> Lua (Optional WorldVariable)
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
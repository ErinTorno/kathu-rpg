{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.Scripting.Lua.Component (registerComponentFunctions) where

import           Apecs
import           Apecs.Physics
import           Data.Text                         (Text)
import qualified Data.Vector                       as Vec
import           Foreign.Lua
import           Linear.V2                         (V2(..))

import           Kathu.Entity.Components
import           Kathu.Graphics.Drawable           (Render(..), RenderSprite(..), switchAnimationByID)
import           Kathu.Scripting.ExternalFunctions
import           Kathu.Util.Apecs
import           Kathu.Util.Types                  (mkIdentifier, unID)

-- ExternalFunctions is unused in this, but is included here since it might be in the future, mirrors the Global's function signature, and acts as a Proxy for g
registerComponentFunctions :: forall w g. (Has w IO Physics, Members w IO (Render g), ReadWriteEach w IO '[Force, Identity, Mass, MovingSpeed, Position, Render g, Tags, Velocity]) => w -> ExternalFunctions w g -> Lua ()
registerComponentFunctions world _ = do
    registerHaskellFunction "getIdentifier"  (getIdentifier world)
    registerHaskellFunction "getName"        (getName world)
    registerHaskellFunction "getDescription" (getDescription world)
    registerHaskellFunction "getTags"        (getTags world)
    registerHaskellFunction "getMovingSpeed" (getMovingSpeed world)
    registerHaskellFunction "setMovingSpeed" (setMovingSpeed world)
    registerHaskellFunction "setAnimation"   (setAnimation (Proxy :: Proxy g) world)
    registerHaskellFunction "getMass"        (getMass world)
    registerHaskellFunction "setMass"        (setMass world)
    registerHaskellFunction "getPosition"    (getPosition world)
    registerHaskellFunction "setPosition"    (setPosition world)
    registerHaskellFunction "getVelocity"    (getVelocity world)
    registerHaskellFunction "setVelocity"    (setVelocity world)
    registerHaskellFunction "getForce"       (getForce world)
    registerHaskellFunction "setForce"       (setForce world)

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
        Just (Tags t) -> Optional $ Just (Vec.toList t)
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
        Just render -> ety $= Render (changeAnim <$> unRender render)

-------------
-- Physics --
-------------

getVector2D :: forall w c. (Get w IO c, Has w IO c, Members w IO c) => (c -> V2 Double) -> w -> Int -> Lua (Optional (Double, Double))
getVector2D mapper !world !etyID = liftIO . Apecs.runWith world $ do
    comp <- getIfExists (Entity etyID)
    pure $ case comp of
        Just vec  -> let (V2 x y) = mapper vec in Optional $ Just (x, y)
        Nothing   -> Optional Nothing

getMass :: forall w. (ReadWrite w IO Mass) => w -> Int -> Lua (Optional Double)
getMass !world !etyID = liftIO . Apecs.runWith world $ do
    mass <- getIfExists (Entity etyID)
    pure $ case mass of
        Just (Mass m) -> Optional $ Just m
        Nothing       -> Optional Nothing

setMass :: forall w. (Has w IO Physics, ReadWrite w IO Position) => w -> Int -> Double-> Lua ()
setMass !world !etyID m = liftIO . Apecs.runWith world $
    Entity etyID $= Mass m

getPosition :: forall w. (ReadWrite w IO Position) => w -> Int -> Lua (Optional (Double, Double))
getPosition = getVector2D $ \(Position v) -> v

setPosition :: forall w. (ReadWrite w IO Position) => w -> Int -> (Double, Double) -> Lua ()
setPosition !world !etyID (x, y) = liftIO . Apecs.runWith world $
    Entity etyID $= Position (V2 x y)

getVelocity :: forall w. (ReadWrite w IO Velocity) => w -> Int -> Lua (Optional (Double, Double))
getVelocity = getVector2D $ \(Velocity v) -> v

setVelocity :: forall w. (ReadWrite w IO Velocity) => w -> Int -> (Double, Double) -> Lua ()
setVelocity !world !etyID (x, y) = liftIO . Apecs.runWith world $
    Entity etyID $= Velocity (V2 x y)

getForce :: forall w. (ReadWrite w IO Force) => w -> Int -> Lua (Optional (Double, Double))
getForce = getVector2D $ \(Force v) -> v

setForce :: forall w. (ReadWrite w IO Force) => w -> Int -> (Double, Double) -> Lua ()
setForce !world !etyID (x, y) = liftIO . Apecs.runWith world $
    Entity etyID $= Force (V2 x y)
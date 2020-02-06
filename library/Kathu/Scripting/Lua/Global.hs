{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.Scripting.Lua.Global (registerGlobalFunctions) where

import           Apecs
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Text                 (Text)
import           Foreign.Lua
import qualified Foreign.Lua.Core          as Lua
import qualified System.Random             as R

import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Entity.Time
import           Kathu.Graphics.Camera
import           Kathu.Scripting.ExternalFunctions
import           Kathu.Scripting.Variables
import           Kathu.Util.Apecs
import           Kathu.Util.Types

registerGlobalFunctions :: forall w g. (ReadWriteEach w IO [Camera, Debug, Local, LogicTime, Random, RenderTime, Variables]) => w -> ExternalFunctions w g -> Lua ()
registerGlobalFunctions world extFuns = do
    registerHaskellFunction "getPlayerEntity" (getPlayerEntity world)
    registerHaskellFunction "getCameraEntity" (getCameraEntity world)
    registerHaskellFunction "setCameraEntity" (setCameraEntity world)
    registerHaskellFunction "isDebug"         (isDebug world)
    registerHaskellFunction "getRandomInt"    (getRandomInt world)
    registerHaskellFunction "getRandomDouble" (getRandomDouble world)
    registerHaskellFunction "getLogicTime"    (getLogicTime world)
    registerHaskellFunction "getRenderTime"   (getRenderTime world)

    registerHaskellFunction "getWorldBool"    (getWorldBool world)
    registerHaskellFunction "getWorldDouble"  (getWorldDouble world)
    registerHaskellFunction "getWorldInt"     (getWorldInt world)
    registerHaskellFunction "getWorldText"    (getWorldText world)
    registerHaskellFunction "getGlobalBool"   (getGlobalBool world)
    registerHaskellFunction "getGlobalDouble" (getGlobalDouble world)
    registerHaskellFunction "getGlobalInt"    (getGlobalInt world)
    registerHaskellFunction "getGlobalText"   (getGlobalText world)

    registerHaskellFunction "setWorldBool"    (setWorldBool world)
    registerHaskellFunction "setWorldDouble"  (setWorldDouble world)
    registerHaskellFunction "setWorldInt"     (setWorldInt world)
    registerHaskellFunction "setWorldText"    (setWorldText world)
    registerHaskellFunction "setGlobalBool"   (setGlobalBool world)
    registerHaskellFunction "setGlobalDouble" (setGlobalDouble world)
    registerHaskellFunction "setGlobalInt"    (setGlobalInt world)
    registerHaskellFunction "setGlobalText"   (setGlobalText world)

    registerHaskellFunction "setPalette"      (setPaletteLua extFuns world)
    registerHaskellFunction "newEntity"       (newFromPrototypeLua extFuns world)
    registerHaskellFunction "destroyEntity"   (destroyEntityLua extFuns world)

setPaletteLua :: ExternalFunctions w g -> w -> Text -> Lua Bool
setPaletteLua extFuns !world !idt = liftIO . Apecs.runWith world $ runW
    where runW = (setPalette extFuns) . mkIdentifier $ idt

newFromPrototypeLua :: ExternalFunctions w g -> w -> Text -> Lua (Optional Int)
newFromPrototypeLua extFuns !world !protoID = liftIO . Apecs.runWith world $ mkEntity
    where mkEntity = (getEntityPrototype extFuns) (mkIdentifier protoID) >>= mkIfPres
          mkIfPres Nothing   = return $ Optional Nothing
          mkIfPres (Just pr) = newFromPrototype extFuns pr
                         >>= return . Optional . Just . unEntity
    
destroyEntityLua :: ExternalFunctions w g -> w -> Int -> Lua ()
destroyEntityLua extFuns !world !ety = liftIO . Apecs.runWith world $ (destroyEntity extFuns) (Entity ety)

-----------------------
-- Unique Components --
-----------------------

getPlayerEntity :: forall w. (ReadWrite w IO Local) => w -> Lua (Optional Int)
getPlayerEntity !world = liftIO . Apecs.runWith world $ do
    player <- getUnique
    case player of
        (Just (_ :: Local, Entity ety)) -> pure . Optional $ Just ety
        Nothing                         -> pure . Optional $ Nothing

getCameraEntity :: forall w. (ReadWrite w IO Camera) => w -> Lua (Optional Int)
getCameraEntity !world = liftIO . Apecs.runWith world $ do
    cam <- getUnique
    case cam of
        (Just (_ :: Camera, Entity ety)) -> pure . Optional $ Just ety
        Nothing                          -> pure . Optional $ Nothing

setCameraEntity :: forall w. (ReadWrite w IO Camera) => w -> Int -> Lua ()
setCameraEntity !world !etyID = liftIO . Apecs.runWith world $ do
    cam <- fromMaybe defaultCamera <$> getUnique
    let deleteCam :: Camera -> Maybe Camera
        deleteCam _ = Nothing
    cmap deleteCam
    (Entity etyID) $= cam

-----------------------
-- Global Components --
-----------------------

isDebug :: forall w. (ReadWrite w IO Debug) => w -> Lua Bool
isDebug !world = liftIO . Apecs.runWith world $ (unDebug <$> get global)

getRandomInt :: forall w. (ReadWrite w IO Random) => w -> Int -> Int -> Lua Int
getRandomInt !world !minV !maxV = liftIO . Apecs.runWith world $ do
    (Random r) <- get global
    let (res, r') = R.random r
    global $= Random r'
    pure (res `mod` (maxV + 1 - minV) + minV)

getRandomDouble :: forall w. (ReadWrite w IO Random) => w -> Lua Double
getRandomDouble !world = liftIO . Apecs.runWith world $ do
    (Random r) <- get global
    let (res, r') = R.random r
    global $= Random r'
    pure res

getLogicTime :: forall w. (ReadWrite w IO LogicTime) => w -> Lua Int
getLogicTime !world = liftIO . Apecs.runWith world $ (fromIntegral . unLogicTime <$> get global)

getRenderTime :: forall w. (ReadWrite w IO RenderTime) => w -> Lua Int
getRenderTime !world = liftIO . Apecs.runWith world $ (fromIntegral . unRenderTime <$> get global)

---------------
-- Variables --
---------------

getVariable :: forall w a. (Pushable a, ReadWrite w IO Variables) => (Identifier -> Variables -> IO (Maybe WorldVariable)) -> (WorldVariable -> Maybe a) -> w -> Text -> Lua (Optional a)
getVariable getGroup matcher !world !idt = liftIO . Apecs.runWith world $ (getVar =<< get global)
    where getVar !vars = do
              var <- liftIO . getGroup (mkIdentifier idt) $ vars
              return $ case var of
                  Just a  -> Optional $ matcher a
                  Nothing -> Optional Nothing

setVariable :: forall w a. (Peekable a, ReadWrite w IO Variables) => (Identifier -> Variables -> Lua Bool) -> (Identifier -> WorldVariable -> Variables -> IO ()) -> (a -> WorldVariable) -> w -> Text -> a -> Lua ()
setVariable getIsValid setter mkVar !world !idtTxt !newVal = do
    variables <- liftIO . Apecs.runWith world $ get global

    let idt  = mkIdentifier idtTxt
    isValid <- getIsValid idt variables

    if isValid then
        liftIO $ setter idt (mkVar newVal) variables
    else
        Lua.throwException ("Did not find variables " ++ show idt ++ " in Variables component")

getWorldBool :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Bool)
getWorldBool = getVariable getWorldVariable $ \case { WorldBool b -> Just b; _ -> Nothing }

getWorldDouble :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Double)
getWorldDouble = getVariable getWorldVariable $ \case { WorldDouble d -> Just d; _ -> Nothing }

getWorldInt :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Int)
getWorldInt = getVariable getWorldVariable $ \case { WorldInt i -> Just (fromIntegral i); _ -> Nothing }

getWorldText :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Text)
getWorldText = getVariable getWorldVariable $ \case { WorldText t -> Just t; _ -> Nothing }

getGlobalBool :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Bool)
getGlobalBool = getVariable getGlobalVariable $ \case { WorldBool b -> Just b; _ -> Nothing }

getGlobalDouble :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Double)
getGlobalDouble = getVariable getGlobalVariable $ \case { WorldDouble d -> Just d; _ -> Nothing }

getGlobalInt :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Int)
getGlobalInt = getVariable getGlobalVariable $ \case { WorldInt i -> Just (fromIntegral i); _ -> Nothing }

getGlobalText :: forall w. (ReadWrite w IO Variables) => w -> Text -> Lua (Optional Text)
getGlobalText = getVariable getGlobalVariable $ \case { WorldText t -> Just t; _ -> Nothing }

setWorldBool :: forall w. (ReadWrite w IO Variables) => w -> Text -> Bool -> Lua ()
setWorldBool = setVariable (\i v -> isJust <$> getWorldVariable i v) setWorldVariable $ WorldBool

setWorldDouble :: forall w. (ReadWrite w IO Variables) => w -> Text -> Double -> Lua ()
setWorldDouble = setVariable (\i v -> isJust <$> getWorldVariable i v) setWorldVariable $ WorldDouble

setWorldInt :: forall w. (ReadWrite w IO Variables) => w -> Text -> Int -> Lua ()
setWorldInt = setVariable (\i v -> isJust <$> getWorldVariable i v) setWorldVariable (WorldInt . fromIntegral)

setWorldText :: forall w. (ReadWrite w IO Variables) => w -> Text -> Text -> Lua ()
setWorldText = setVariable (\i v -> isJust <$> getWorldVariable i v) setWorldVariable WorldText

-- Global variables don't care if the key is present or not before they can be set

setGlobalBool :: forall w. (ReadWrite w IO Variables) => w -> Text -> Bool -> Lua ()
setGlobalBool = setVariable (\_ _ -> pure True) setGlobalVariable WorldBool

setGlobalDouble :: forall w. (ReadWrite w IO Variables) => w -> Text -> Double -> Lua ()
setGlobalDouble = setVariable (\_ _ -> pure True) setGlobalVariable WorldDouble

setGlobalInt :: forall w. (ReadWrite w IO Variables) => w -> Text -> Int -> Lua ()
setGlobalInt = setVariable (\_ _ -> pure True) setGlobalVariable (WorldInt . fromIntegral)

setGlobalText :: forall w. (ReadWrite w IO Variables) => w -> Text -> Text -> Lua ()
setGlobalText = setVariable (\_ _ -> pure True) setGlobalVariable WorldText
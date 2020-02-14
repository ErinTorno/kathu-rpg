{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.Scripting.Lua.Global (registerGlobalFunctions) where

import           Apecs
import           Control.Monad             (when)
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Text                 (Text)
import           Foreign.Lua
import qualified System.Random             as R

import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Entity.Time
import           Kathu.Graphics.Camera
import           Kathu.Scripting.ExternalFunctions
import           Kathu.Scripting.Lua.Types
import           Kathu.Scripting.Variables
import           Kathu.Util.Apecs
import           Kathu.Util.Collection     (fromJustElseError)
import           Kathu.Util.Types

registerGlobalFunctions :: forall w g. (ReadWriteEach w IO [ActiveScript, Camera, Debug, Local, LogicTime, Random, RenderTime, RunningScriptEntity, ScriptEventBuffer, Variables]) => w -> ExternalFunctions w g -> Lua ()
registerGlobalFunctions world extFuns = do
    registerHaskellFunction "getPlayerEntity" (getPlayerEntity world)
    registerHaskellFunction "getCameraEntity" (getCameraEntity world)
    registerHaskellFunction "setCameraEntity" (setCameraEntity world)
    registerHaskellFunction "getScriptEntity" (getScriptEntity world)
    registerHaskellFunction "isDebug"         (isDebug world)
    registerHaskellFunction "getRandomInt"    (getRandomInt world)
    registerHaskellFunction "getRandomDouble" (getRandomDouble world)
    registerHaskellFunction "getLogicTime"    (getLogicTime world)
    registerHaskellFunction "getRenderTime"   (getRenderTime world)

    registerHaskellFunction "getWorldVar"     (getVariable getWorldVariable world)
    registerHaskellFunction "getGlobalVar"    (getVariable getGlobalVariable world)

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

    registerHaskellFunction "registerGlobalVarListener" (registerListener addGlobalListener world)
    registerHaskellFunction "registerWorldVarListener"  (registerListener addWorldListener world)

setPaletteLua :: ExternalFunctions w g -> w -> Text -> Lua Bool
setPaletteLua extFuns !world !idt = liftIO . Apecs.runWith world $ runW
    where runW = setPalette extFuns . mkIdentifier $ idt

newFromPrototypeLua :: ExternalFunctions w g -> w -> Text -> Lua (Optional Int)
newFromPrototypeLua extFuns !world !protoID = liftIO . Apecs.runWith world $ mkEntity
    where mkEntity = getEntityPrototype extFuns (mkIdentifier protoID) >>= mkIfPres
          mkIfPres Nothing   = return $ Optional Nothing
          mkIfPres (Just pr) = Optional . Just . unEntity <$> newFromPrototype extFuns pr
    
destroyEntityLua :: ExternalFunctions w g -> w -> Int -> Lua ()
destroyEntityLua extFuns !world !ety = liftIO . Apecs.runWith world $ destroyEntity extFuns (Entity ety)

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
    Entity etyID $= cam

-----------------------
-- Global Components --
-----------------------

getScriptEntity :: forall w. (ReadWrite w IO RunningScriptEntity) => w -> Lua (Optional Int)
getScriptEntity !world = liftIO . Apecs.runWith world $ (Optional . fmap unEntity . runningScript <$> get global)

isDebug :: forall w. (ReadWrite w IO Debug) => w -> Lua Bool
isDebug !world = liftIO . Apecs.runWith world $ (unDebug <$> get global)

getRandomInt :: forall w. (ReadWrite w IO Random) => w -> Int -> Int -> Lua Int
getRandomInt !world !minV !maxV = liftIO . Apecs.runWith world $ do
    Random r <- get global
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

registerListener :: forall w. (ReadWriteEach w IO [ActiveScript, RunningScriptEntity, ScriptEventBuffer, Variables])
                 => (Identifier -> Int -> (WorldVariable -> IO ()) -> Variables -> SystemT w IO ()) -> w -> Text -> String -> Lua ()
registerListener addWatch !world !idt fnName = liftIO . Apecs.runWith world $ do
    let getRS = fromJustElseError "RunningScriptEntity was Nothing when registerListener was called" . runningScript
    currentEty   <- getRS <$> get global
    variables    <- get global
    activeScript <- getIfExists currentEty
    case activeScript of
        Nothing     -> return ()
        Just script -> do
            let ety = unEntity currentEty

            -- we add this event into the buffer to run as soon as this script instance finishes
            let execFn wv   = Apecs.runWith world (execFor script $ callFunc fnName ety wv)
                onUpdate wv = Apecs.runWith world $ do
                    ScriptEventBuffer buffer <- get global
                    global $= ScriptEventBuffer (execFn wv : buffer)

            addWatch (mkIdentifier idt) ety onUpdate variables

getVariable :: forall w. (ReadWrite w IO Variables) => (Identifier -> Variables -> IO (Maybe WorldVariable)) -> w -> Text -> Lua (Optional WorldVariable)
getVariable getGroup !world !idt = liftIO . Apecs.runWith world $ (getVar =<< get global)
    where getVar !vars = do
              var <- liftIO . getGroup (mkIdentifier idt) $ vars
              return $ case var of
                  Just a  -> Optional $ Just a
                  Nothing -> Optional Nothing

setVariableLua :: forall w a. (Peekable a, ReadWrite w IO Variables) => (Identifier -> Variables -> SystemT w IO Bool) -> (Identifier -> WorldVariable -> Variables -> IO ()) -> (a -> WorldVariable) -> w -> Text -> a -> Lua ()
setVariableLua getIsValid setter mkVar !world !idtTxt !newVal = liftIO . Apecs.runWith world $ do
    variables <- liftIO . Apecs.runWith world $ get global

    let idt  = mkIdentifier idtTxt
    isValid <- getIsValid idt variables

    when isValid $
        liftIO $ setter idt (mkVar newVal) variables

setWorldBool :: forall w. (ReadWrite w IO Variables) => w -> Text -> Bool -> Lua ()
setWorldBool = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable WorldBool

setWorldDouble :: forall w. (ReadWrite w IO Variables) => w -> Text -> Double -> Lua ()
setWorldDouble = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable WorldDouble

setWorldInt :: forall w. (ReadWrite w IO Variables) => w -> Text -> Int -> Lua ()
setWorldInt = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable (WorldInt . fromIntegral)

setWorldText :: forall w. (ReadWrite w IO Variables) => w -> Text -> Text -> Lua ()
setWorldText = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable WorldText

-- Global variables don't care if the key is present or not before they can be set

setGlobalBool :: forall w. (ReadWrite w IO Variables) => w -> Text -> Bool -> Lua ()
setGlobalBool = setVariableLua (\_ _ -> pure True) setGlobalVariable WorldBool

setGlobalDouble :: forall w. (ReadWrite w IO Variables) => w -> Text -> Double -> Lua ()
setGlobalDouble = setVariableLua (\_ _ -> pure True) setGlobalVariable WorldDouble

setGlobalInt :: forall w. (ReadWrite w IO Variables) => w -> Text -> Int -> Lua ()
setGlobalInt = setVariableLua (\_ _ -> pure True) setGlobalVariable (WorldInt . fromIntegral)

setGlobalText :: forall w. (ReadWrite w IO Variables) => w -> Text -> Text -> Lua ()
setGlobalText = setVariableLua (\_ _ -> pure True) setGlobalVariable WorldText
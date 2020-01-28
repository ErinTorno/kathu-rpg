{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.Scripting.Lua.Global (registerGlobalFunctions) where

import           Apecs
import           Data.Text                 (Text)
import           Foreign.Lua
import qualified System.Random             as R

import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Entity.Time
import           Kathu.Graphics.Camera
import           Kathu.Scripting.Lua.Types
import           Kathu.Util.Apecs
import           Kathu.Util.Types

registerGlobalFunctions :: forall w. (ReadWriteEach w IO [Camera, Local, LogicTime, Random, RenderTime]) => w -> ExternalFunctions w -> Lua ()
registerGlobalFunctions world extFuns = do
    registerHaskellFunction "getPlayerEntity" (getPlayerEntity world)
    registerHaskellFunction "getCameraEntity" (getCameraEntity world)
    registerHaskellFunction "getRandomInt"    (getRandomInt world)
    registerHaskellFunction "getRandomDouble" (getRandomDouble world)
    registerHaskellFunction "getLogicTime"    (getLogicTime world)
    registerHaskellFunction "getRenderTime"   (getRenderTime world)

    registerHaskellFunction "setPalette"      (setPaletteLua extFuns world)

setPaletteLua :: ExternalFunctions w -> w -> Text -> Lua Bool
setPaletteLua extFuns !world !idt = liftIO . Apecs.runWith world $ runW
    where runW = (setPalette extFuns) . mkIdentifier $ idt

-----------------------
-- Unique Components --
-----------------------

getPlayerEntity :: forall w. (ReadWrite w IO Local) => w -> Lua (Optional Int)
getPlayerEntity !world = liftIO . Apecs.runWith world $ do
    comp <- getUnique
    case comp of
        (Just (_ :: Local, Entity ety)) -> pure . Optional $ Just ety
        Nothing                         -> pure . Optional $ Nothing

getCameraEntity :: forall w. (ReadWrite w IO Camera) => w -> Lua (Optional Int)
getCameraEntity !world = liftIO . Apecs.runWith world $ do
    comp <- getUnique
    case comp of
        (Just (_ :: Camera, Entity ety)) -> pure . Optional $ Just ety
        Nothing                          -> pure . Optional $ Nothing

-----------------------
-- Global Components --
-----------------------

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
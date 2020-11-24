module Kathu.Scripting.Lua.Global (registerGlobalFunctions) where

import           Apecs
import           Control.Lens
import           Control.Monad             (forM_, when)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Text                 (Text)
import           Foreign.Lua
import           Linear.V2                 (V2(..))
import qualified System.Random             as R
import           Verda.Event.Controls      (CursorMotionState, cursorPosition)
import           Verda.Graphics.Components (Camera(..), defaultCamera)
import           Verda.Logger
import           Verda.Time
import           Verda.Util.Containers     (fromJustElseError)
import           Verda.Util.Types
import           Verda.Util.Apecs
import           Verda.World               (IsDebug(..))

import           Kathu.Config.Dictionary
import           Kathu.Entity.Item         (ItemStack(..))
import           Kathu.Entity.Components
import           Kathu.Entity.System
import           Kathu.Random
import           Kathu.Scripting.Lua.Types
import           Kathu.Scripting.Variables
import           Kathu.World.WorldSpace    (WorldInventory(..), WorldSpace(..))

registerGlobalFunctions :: KathuWorld -> Lua ()
registerGlobalFunctions  world = do
    registerHaskellFunction "log"               $ logLua world
    registerHaskellFunction "getCursorPosition" $ getCursorPosition world
    registerHaskellFunction "getPlayerEntity"   $ getPlayerEntity world
    registerHaskellFunction "getCameraEntity"   $ getCameraEntity world
    registerHaskellFunction "setCameraEntity"   $ setCameraEntity world
    registerHaskellFunction "getScriptEntity"   $ getScriptEntity world
    registerHaskellFunction "isDebug"           $ isDebug world
    registerHaskellFunction "getRandomInt"      $ getRandomInt world
    registerHaskellFunction "getRandomDouble"   $ getRandomDouble world
    registerHaskellFunction "getLogicTime"      $ getLogicTime world
    registerHaskellFunction "getRenderTime"     $ getRenderTime world

    registerHaskellFunction "getWorldVar"       $ getVariable getWorldVariable world
    registerHaskellFunction "getGlobalVar"      $ getVariable getGlobalVariable world

    registerHaskellFunction "setWorldBool"      $ setWorldBool world
    registerHaskellFunction "setWorldDouble"    $ setWorldDouble world
    registerHaskellFunction "setWorldInt"       $ setWorldInt world
    registerHaskellFunction "setWorldText"      $ setWorldText world
    registerHaskellFunction "setGlobalBool"     $ setGlobalBool world
    registerHaskellFunction "setGlobalDouble"   $ setGlobalDouble world
    registerHaskellFunction "setGlobalInt"      $ setGlobalInt world
    registerHaskellFunction "setGlobalText"     $ setGlobalText world

    registerHaskellFunction "setPalette"        $ setPaletteLua world
    registerHaskellFunction "newEntity"         $ newFromPrototypeLua world

    registerHaskellFunction "getCurrentWorldspaceID"   $ getCurrentWorldspaceID world
    registerHaskellFunction "addWorldInventoryItem"    $ addWorldInventoryItem world
    registerHaskellFunction "getWorldSpaceInventoryID" $ getWorldSpaceInventoryID world
    registerHaskellFunction "hasWorldInventoryItem"    $ hasWorldInventoryItem world
    registerHaskellFunction "removeWorldInventoryItem" $ removeWorldInventoryItem world

    registerHaskellFunction "registerGlobalVarListener" $ registerListener addGlobalListener world
    registerHaskellFunction "registerWorldVarListener"  $ registerListener addWorldListener world


logLua :: forall w. Get w IO Logger => w -> Text -> Lua ()
logLua !world t = liftIO . Apecs.runWith world $ logLine Info t

getCursorPosition :: forall w. (ReadWrite w IO CursorMotionState) => w -> Lua (V2 Double)
getCursorPosition !world = liftIO . Apecs.runWith world $
    cursorPosition <$> get global

setPaletteLua :: KathuWorld -> Text -> Lua Bool
setPaletteLua !world !idt = liftIO . Apecs.runWith world $ runW
    where runW = setPalette . mkIdentifier $ idt

newFromPrototypeLua :: KathuWorld -> Text -> Lua (Optional Int)
newFromPrototypeLua !world !protoID = liftIO . Apecs.runWith world $ mkEntity
    where mkEntity = lookupEntityPrefab (mkIdentifier protoID) >>= mkIfPres
          mkIfPres Nothing   = return $ Optional Nothing
          mkIfPres (Just pr) = Optional . Just . unEntity <$> newFromPrefab pr

-----------------------
-- Unique Components --
-----------------------

getPlayerEntity :: KathuWorld -> Lua (Optional Int)
getPlayerEntity !world = liftIO . Apecs.runWith world $ do
    player <- getUnique
    case player of
        (Just (_ :: Local, Entity ety)) -> pure . Optional $ Just ety
        Nothing                         -> pure . Optional $ Nothing

getCameraEntity :: KathuWorld -> Lua (Optional Int)
getCameraEntity !world = liftIO . Apecs.runWith world $ do
    cam <- getUnique
    case cam of
        (Just (_ :: Camera, Entity ety)) -> pure . Optional $ Just ety
        Nothing                          -> pure . Optional $ Nothing

setCameraEntity :: KathuWorld -> Int -> Lua ()
setCameraEntity !world !etyID = liftIO . Apecs.runWith world $ do
    cam <- fromMaybe defaultCamera <$> getUnique
    let deleteCam :: Camera -> Maybe Camera
        deleteCam _ = Nothing
    cmap deleteCam
    Entity etyID $= cam

-----------------------
-- Global Components --
-----------------------

getScriptEntity :: KathuWorld -> Lua (Optional Int)
getScriptEntity !world = liftIO . Apecs.runWith world $ (Optional . fmap unEntity . runningScript <$> get global)

isDebug :: KathuWorld -> Lua Bool
isDebug !world = liftIO . Apecs.runWith world $ (unDebug <$> get global)

getRandomInt :: KathuWorld -> Int -> Int -> Lua Int
getRandomInt !world !minV !maxV = liftIO . Apecs.runWith world $ do
    Random r <- get global
    let (res, r') = R.random r
    global $= Random r'
    pure (res `mod` (maxV + 1 - minV) + minV)

getRandomDouble :: KathuWorld -> Lua Double
getRandomDouble !world = liftIO . Apecs.runWith world $ do
    (Random r) <- get global
    let (res, r') = R.random r
    global $= Random r'
    pure res

getLogicTime :: KathuWorld -> Lua Int
getLogicTime !world = liftIO . Apecs.runWith world $ (fromIntegral . unLogicTime <$> get global)

getRenderTime :: KathuWorld -> Lua Int
getRenderTime !world = liftIO . Apecs.runWith world $ (fromIntegral . unRenderTime <$> get global)

getCurrentWorldspaceID :: KathuWorld -> Lua Identifier
getCurrentWorldspaceID !world = liftIO . Apecs.runWith world $ (_worldID <$> get global)

getWorldSpaceField :: (WorldSpace -> a) -> KathuWorld -> Identifier -> Lua (Optional a)
getWorldSpaceField f !world !worldID = liftIO . Apecs.runWith world $ do
    dict <- get global
    pure $ case dict^.dictWorldSpaces.to (Map.lookup worldID) of
        Nothing -> Optional Nothing
        Just ws -> Optional . Just . f $ ws

getWorldSpaceInventoryID :: KathuWorld -> Identifier -> Lua (Optional Identifier)
getWorldSpaceInventoryID = getWorldSpaceField _worldInventory

hasWorldInventoryItem :: KathuWorld -> Identifier -> Identifier -> Int -> Lua Bool
hasWorldInventoryItem !world !worldInvID !itemID !count = liftIO . Apecs.runWith world $ do
    WorldInventory worldInv <- get global
    pure $ case Map.lookup worldInvID worldInv of
        Nothing  -> False
        Just inv -> case Map.lookup itemID inv of
            Nothing              -> False
            Just (ItemStack _ n) -> n >= count

addWorldInventoryItem :: KathuWorld -> Identifier -> Identifier -> Int -> Lua ()
addWorldInventoryItem !world !worldInvID !itemID !count = liftIO . Apecs.runWith world $ do
    dict <- get global
    WorldInventory worldInv <- get global
    forM_ (Map.lookup worldInvID worldInv) $ \inv ->
        case dict^.dictItems.to (Map.lookup itemID) of
            Nothing   -> pure ()
            Just item ->
                let addItem Nothing                = if count <= 0     then Nothing else Just $ ItemStack item count
                    addItem (Just (ItemStack _ n)) = if n + count <= 0 then Nothing else Just $ ItemStack item (count + n)
                    inv' = Map.alter addItem itemID inv
                 in global $= WorldInventory (Map.insert worldInvID inv' worldInv)

removeWorldInventoryItem :: KathuWorld -> Identifier -> Identifier -> Int -> Lua ()
removeWorldInventoryItem !world !worldInvID !itemID = addWorldInventoryItem world worldInvID itemID . negate

---------------
-- Variables --
---------------

registerListener :: (Identifier -> Int -> (WorldVariable -> IO ()) -> Variables -> SystemT' IO ()) -> KathuWorld -> Text -> String -> Lua ()
registerListener addWatch !world !idt fnName = liftIO . Apecs.runWith world $ do
    let getRS = fromJustElseError "RunningScriptEntity was Nothing when registerListener was called" . runningScript
    currentEty   <- getRS <$> get global
    variables    <- get global
    activeScript <- getIfExists currentEty

    forM_ activeScript $ \script -> do
            let ety = unEntity currentEty

            -- we add this event into the buffer to run as soon as this script instance finishes
            let execFn wv   = Apecs.runWith world (execFor script $ callFunc fnName ety wv)
                onUpdate wv = Apecs.runWith world $ do
                    ScriptEventBuffer buffer <- get global
                    global $= ScriptEventBuffer (execFn wv : buffer)

            addWatch (mkIdentifier idt) ety onUpdate variables

getVariable :: (Identifier -> Variables -> IO (Maybe WorldVariable)) -> KathuWorld -> Text -> Lua (Optional WorldVariable)
getVariable getGroup !world !idt = liftIO . Apecs.runWith world $ (getVar =<< get global)
    where getVar !vars = do
              var <- liftIO . getGroup (mkIdentifier idt) $ vars
              return $ case var of
                  Just a  -> Optional $ Just a
                  Nothing -> Optional Nothing

setVariableLua :: (Identifier -> Variables -> SystemT' IO Bool) -> (Identifier -> WorldVariable -> Variables -> IO ()) -> (a -> WorldVariable) -> KathuWorld -> Text -> a -> Lua ()
setVariableLua getIsValid setter mkVar !world !idtTxt !newVal = liftIO . Apecs.runWith world $ do
    variables <- get global

    let idt  = mkIdentifier idtTxt
    isValid <- getIsValid idt variables

    when isValid $
        liftIO $ setter idt (mkVar newVal) variables

setWorldBool :: KathuWorld -> Text -> Bool -> Lua ()
setWorldBool = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable WorldBool

setWorldDouble :: KathuWorld -> Text -> Double -> Lua ()
setWorldDouble = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable WorldDouble

setWorldInt :: KathuWorld -> Text -> Int -> Lua ()
setWorldInt = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable (WorldInt . fromIntegral)

setWorldText :: KathuWorld -> Text -> Text -> Lua ()
setWorldText = setVariableLua (\i v -> isJust <$> getWorldVariable i v) setWorldVariable WorldText

-- Global variables don't care if the key is present or not before they can be set

setGlobalBool :: KathuWorld -> Text -> Bool -> Lua ()
setGlobalBool = setVariableLua (\_ _ -> pure True) setGlobalVariable WorldBool

setGlobalDouble :: KathuWorld -> Text -> Double -> Lua ()
setGlobalDouble = setVariableLua (\_ _ -> pure True) setGlobalVariable WorldDouble

setGlobalInt :: KathuWorld -> Text -> Int -> Lua ()
setGlobalInt = setVariableLua (\_ _ -> pure True) setGlobalVariable (WorldInt . fromIntegral)

setGlobalText :: KathuWorld -> Text -> Text -> Lua ()
setGlobalText = setVariableLua (\_ _ -> pure True) setGlobalVariable WorldText
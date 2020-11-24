{-# OPTIONS_GHC -fno-warn-orphans #-}
-- We add Lua Peekable and Pushable instances here for commonly used types to make them easier to use
-- Wrapping them in a newtype would defeat this purpose

{-# LANGUAGE TemplateHaskell      #-}

-- Meant to be imported qualified

module Kathu.Scripting.Lua.Types where

import           Apecs
import           Control.Concurrent.MVar
import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Lens                hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types            (typeMismatch)
import           Data.Functor.Compose
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Foreign.Lua                 (Lua)
import qualified Foreign.Lua                 as Lua
import           Data.Vector                 (Vector)
import           Linear.V2                   (V2(..))

import           Verda.IO.Directory
import           Verda.Parsing.Aeson
import           Kathu.Scripting.Event
import           Kathu.Scripting.Variables   (WatchedVariable, WorldVariable)
import           Verda.Util.Apecs
import           Verda.Util.Dependency
import           Verda.Util.Types

data Script = Script
    { _scriptID         :: !Identifier -- a unique identifier to refer to this script (usually its file path)
    , _mainScript       :: !ByteString -- this is main script
    , _scriptEventFlags :: !EventFlag
    , _isSingleton      :: !Bool       -- if True, then there all users of this script share the same instance and state
    } deriving Eq
makeLenses ''Script

blankScript :: Script
blankScript = Script "" BS.empty noEventFlags False

sanitizedScriptID :: Script -> Text
sanitizedScriptID Script{_scriptID = sID} = fromMaybe txtID . T.stripPrefix assetPath $ txtID
    where txtID = unID sID

instance ToJSON Script where
    toJSON script@(Script _ _ flags isSingle)
        -- If everything else can be blank, just serialize this to a string
        | flags == noEventFlags && not isSingle = toJSON sanitizedFile
        | otherwise = object
            [ "file"         .= sanitizedFile
            , "events"       .= nothingUnless (flags /= noEventFlags) flags
            , "is-singleton" .= nothingUnless isSingle True
            ]
        where nothingUnless cond val = if cond then Just val else Nothing 
              -- when saved, assets is the root, so if it keeps the assets, it will parse it wrong
              sanitizedFile = sanitizedScriptID script

instance (s `CanProvide` WorkingDirectory, MonadIO m) => FromJSON (Dependency s m Script) where
    parseJSON e = let readF f = do path <- (resolveAssetPathDP . T.unpack) f
                                   bstr <- liftDependency . liftIO . BS.readFile $ path
                                   pure $ Script (mkIdentifier . T.pack $ path) bstr
                   in case e of
        (String s) -> pure $ readF s <*> pure noEventFlags <*> pure False
        (Object v) -> getCompose $ Compose (readF <$> v .: "file")
                  <*> v .:^? "events" .!=- noEventFlags
                  <*> v .:^? "is-singleton" .!=- False
        v          -> typeMismatch "Script" v

data SingletonStatus
    = NonSingleton       -- separate state for each instance of the script
    | SingletonReference -- this script holds a reference to a "master" script instance that all share
    | SingletonBase      -- this is the master instance
    deriving (Show, Eq)

data ActiveScript = ActiveScript
    { activeState       :: !(MVar Lua.State)
    , eventFlags        :: !EventFlag
    , instanceEntity    :: !Entity
    , watchedVariables  :: !(Vector WatchedVariable)
    , wireControllers   :: !(Vector Identifier)      -- this script can attempt to send signals to entities holding these wire identifiers
    , wireReceivers     :: !(Vector Identifier)
    , singletonStatus   :: !SingletonStatus
    , instanceConfig    :: !(IDMap WorldVariable)
    }

instance Component ActiveScript where type Storage ActiveScript = Map ActiveScript

newtype ScriptBank = ScriptBank {unScriptBank :: IDHashTable ActiveScript }

instance Semigroup ScriptBank where (<>) = mappend
instance Monoid ScriptBank where mempty = error "Attempted to use ScriptBank before it has been loaded"
instance Component ScriptBank where type Storage ScriptBank = Global ScriptBank

newtype RunningScriptEntity = RunningScriptEntity {runningScript :: Maybe Entity}

instance Semigroup RunningScriptEntity where (<>) = mappend
instance Monoid RunningScriptEntity where mempty = RunningScriptEntity Nothing
instance Component RunningScriptEntity where type Storage RunningScriptEntity = Global RunningScriptEntity

-- | A buffer for IO events that scripts generate; these are executed after the script runs to prevent concurrency issues with same script instances
newtype ScriptEventBuffer = ScriptEventBuffer {unBuffer :: [IO ()]}

instance Semigroup ScriptEventBuffer where (<>) = mappend
instance Monoid ScriptEventBuffer where mempty = ScriptEventBuffer []
instance Component ScriptEventBuffer where type Storage ScriptEventBuffer = Global ScriptEventBuffer

newtype LuaModules = LuaModules {unLuaModules :: [Lua ()]}

instance Semigroup LuaModules where (<>) = mappend
instance Monoid LuaModules where mempty = LuaModules []
instance Component LuaModules where type Storage LuaModules = Global LuaModules

type LuaModule w = w -> Lua ()

handleLua :: a -> Lua a -> Lua a
handleLua !def !lua = do
    res <- Lua.try lua
    case res of
        Left exc  -> liftIO (print exc) >> pure def
        Right ret -> pure ret

handleLuaOp :: Lua a -> Lua ()
handleLuaOp !lua = do
    res <- Lua.try lua
    case res of
        Left exc -> liftIO (print exc)
        Right _  -> pure ()

execFor :: forall w. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer]) => ActiveScript -> Lua () -> SystemT w IO ()
execFor ActiveScript {activeState = stmvar, instanceEntity = ety} fn = do
    global $= RunningScriptEntity (Just ety)

    liftIO $ do
        lstate <- takeMVar stmvar

        newSt  <- Lua.runWith lstate (handleLuaOp fn >> Lua.state)

        putMVar stmvar newSt

    ScriptEventBuffer buffer <- get global
    global $= ScriptEventBuffer []
    liftIO $ forM_ buffer id

    global $= RunningScriptEntity Nothing

runFor :: forall w a. (ReadWriteEach w IO [RunningScriptEntity, ScriptEventBuffer]) => ActiveScript -> Lua a -> SystemT w IO (Maybe a)
runFor ActiveScript {activeState = stmvar, instanceEntity = ety} fn = do
    global $= RunningScriptEntity (Just ety)

    a <- liftIO $ do
        lstate     <- takeMVar stmvar

        (a, newSt) <- Lua.runWith lstate ((,) <$> handleLua Nothing (Just <$> fn) <*> Lua.state)

        putMVar stmvar newSt
        return a

    ScriptEventBuffer buffer <- get global
    liftIO $ forM_ buffer id
    global $= ScriptEventBuffer []

    global $= RunningScriptEntity Nothing
    return a

shouldScriptRun :: ScriptEvent -> ActiveScript -> Bool
shouldScriptRun e = isEventSet e . eventFlags

setInstanceConfig :: IDMap WorldVariable -> ActiveScript -> ActiveScript
setInstanceConfig config script = script {instanceConfig = config}

-- Peekable/Pushable instances

instance Lua.Peekable Identifier where
    peek idx = mkIdentifier <$> Lua.peek idx

instance Lua.Pushable Identifier where
    push (Identifier idt _) = Lua.push idt

instance Lua.Peekable Entity where
    peek idx = Entity <$> Lua.peek idx

instance Lua.Pushable Entity where
    push (Entity ety) = Lua.push ety

instance Lua.Peekable a => Lua.Peekable (V2 a) where
    peek idx = uncurry V2 <$> Lua.peek idx

instance Lua.Pushable a => Lua.Pushable (V2 a) where
    push (V2 x y) = Lua.push (x, y)
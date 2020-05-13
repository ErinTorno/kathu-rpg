module Kathu.Scripting.Variables where

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.ST        (stToIO)
import           Data.Aeson
import           Data.Aeson.Types        (Parser, typeMismatch)
import qualified Data.HashTable.ST.Basic as HT
import           Data.Int
import qualified Data.Map                as Map
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import           Data.Serialize
import           Data.Text               (Text)
import qualified Foreign.Lua             as Lua
import           GHC.Generics

import           Kathu.Util.Types        (Identifier, IDHashTable, IDMap)

data VariableType = WorldVar | GlobalVar

-- | Config-defined variables accessabled by scripts; these are serialized and saved with the game's state, unlike script instance variables
data WorldVariable
    = WorldBool   !Bool
    | WorldDouble !Double
    | WorldInt    !Int64
    | WorldText   !Text
    | WorldUnit -- Only used for dynamically type variables without any value
    deriving (Show, Eq, Generic)

instance Serialize WorldVariable

instance ToJSON WorldVariable where
    toJSON (WorldBool b)   = object ["type" .= ("bool"   :: Text), "value" .= b]
    toJSON (WorldDouble d) = object ["type" .= ("double" :: Text), "value" .= d]
    toJSON (WorldInt i)    = object ["type" .= ("int"    :: Text), "value" .= i]
    toJSON (WorldText t)   = object ["type" .= ("text"   :: Text), "value" .= t]
    toJSON WorldUnit       = object ["type" .= ("unit"   :: Text)]

instance FromJSON WorldVariable where
    parseJSON (Object v)   = v .: "type" >>= var
        where var :: Text -> Parser WorldVariable
              var "bool"   = WorldBool   <$> v .:? "value" .!= False
              var "double" = WorldDouble <$> v .:? "value" .!= 0
              var "int"    = WorldInt    <$> v .:? "value" .!= 0
              var "text"   = WorldText   <$> v .:? "value" .!= ""
              var "unit"   = WorldText   <$> v .:? "value" .!= ""
              var e        = fail ("Unable to parse world variable with type of " ++ show e)
    parseJSON e            = typeMismatch "WorldVariable" e

-- We can't peek since we might not be able to tell its type
instance Lua.Pushable WorldVariable where
    push var = case var of
        WorldBool b   -> Lua.push b
        WorldDouble d -> Lua.push d
        WorldInt i    -> Lua.push (fromIntegral i :: Int)
        WorldText t   -> Lua.push t
        WorldUnit     -> Lua.push ()

type OnVariableChange = WorldVariable -> IO ()

data WatchableVariable = WatchableVariable
    { worldVariable :: !WorldVariable
    , varListeners  :: !(IntMap OnVariableChange)
    }

data WatchedVariable = WatchedVariable
    { variableID   :: !Identifier
    , variableType :: !VariableType
    }

----------------
-- Components --
----------------

initialVariablesSize :: Int
initialVariablesSize = 32

data Variables = Variables
    { globalVariables     :: !(IDHashTable WatchableVariable)
    , worldspaceVariables :: !(IDHashTable WatchableVariable)
    }

initVariables :: MonadIO m => m Variables
initVariables = do
    globalHT <- liftIO . stToIO $ HT.newSized initialVariablesSize
    worldHT  <- liftIO . stToIO $ HT.newSized initialVariablesSize
    return (Variables globalHT worldHT)

mutateVariable :: MonadIO m => Identifier -> IDHashTable WatchableVariable -> (Maybe WatchableVariable -> Maybe WatchableVariable) -> m ()
mutateVariable idt table f = liftIO . stToIO $ HT.mutate table idt ((,()) . f)

getGlobalVariable :: MonadIO m => Identifier -> Variables -> m (Maybe WorldVariable)
getGlobalVariable idt (Variables ht _) = liftIO . stToIO $ (fmap worldVariable <$> HT.lookup ht idt)

getWorldVariable :: MonadIO m => Identifier -> Variables -> m (Maybe WorldVariable)
getWorldVariable idt (Variables _ ht) = liftIO . stToIO $ (fmap worldVariable <$> HT.lookup ht idt)

setVariable :: MonadIO m => (Variables -> IDHashTable WatchableVariable) -> Identifier -> WorldVariable -> Variables -> m ()
setVariable getGroup idt newVal vars = do
    -- we update it and return the listeners
    let updateVar Nothing     = (Just $ WatchableVariable newVal IntMap.empty, IntMap.empty)
        updateVar (Just (WatchableVariable _ ls)) = (Just $ WatchableVariable newVal ls, ls)
    
    listeners <- liftIO . stToIO $ HT.mutate (getGroup vars) idt updateVar
    -- runs all event handlers
    liftIO $ forM_ listeners ($newVal)

setGlobalVariable :: MonadIO m => Identifier -> WorldVariable -> Variables -> m ()
setGlobalVariable = setVariable globalVariables

setWorldVariable :: MonadIO m => Identifier -> WorldVariable -> Variables -> m ()
setWorldVariable = setVariable worldspaceVariables

replaceWorldVariables :: MonadIO m => Variables -> IDMap WorldVariable -> m (IDMap WorldVariable)
replaceWorldVariables variables !newVars = liftIO . stToIO $ do
    let worldVars = worldspaceVariables variables
    -- we convert the previous HashTable into just a normal Map
    prevWorldMap <- Map.fromList <$> HT.foldM (\acc (k, v) -> return ((k, worldVariable v) : acc)) [] worldVars

    HT.mapM_ (\(k, _) -> HT.delete worldVars k) worldVars

    forM_ (Map.assocs newVars) $ \(k, v) -> HT.insert worldVars k (WatchableVariable v IntMap.empty)

    return prevWorldMap

addListener :: MonadIO m => Bool -> (Variables -> IDHashTable WatchableVariable) -> Identifier -> Int -> OnVariableChange -> Variables -> m ()
addListener createIfMissing getGroup idt iid onUpdate vars = mutateVariable idt (getGroup vars) $ \case
    Just (WatchableVariable var listeners) -> Just $ WatchableVariable var (IntMap.insert iid onUpdate listeners)
    Nothing -> if createIfMissing
               then Just $ WatchableVariable WorldUnit (IntMap.singleton iid onUpdate)
               else Nothing

addGlobalListener :: MonadIO m => Identifier -> Int -> OnVariableChange -> Variables -> m ()
addGlobalListener = addListener True globalVariables

addWorldListener :: MonadIO m => Identifier -> Int -> OnVariableChange -> Variables -> m ()
addWorldListener = addListener False worldspaceVariables

deleteListener :: MonadIO m => Int -> Variables -> WatchedVariable -> m ()
deleteListener iid vars (WatchedVariable idt typ) = liftIO . stToIO $ HT.mutate table idt ((,()) . updateVar)
    where updateVar Nothing = Nothing
          updateVar (Just (WatchableVariable var listeners)) = Just $ WatchableVariable var (IntMap.delete iid listeners)
          table = case typ of
              GlobalVar -> globalVariables vars
              WorldVar  -> worldspaceVariables vars
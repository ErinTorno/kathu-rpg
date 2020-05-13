module Kathu.Scripting.Event
    ( ScriptEvent
    , EventFlag
    , allEvents
    , noEventFlags
    , mkEventFlags
    , setEventFlagEnabled
    , isEventSet
    , onUpdate
    , onInit
    , onDestroy
    , onSignalChange
    , onSensorCollisionBegin
    , onSensorCollisionEnd
    ) where

import           Data.Aeson
import           Data.Bits
import           Data.Text             (Text)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Word
import qualified Data.Foldable         as F

import           Kathu.Util.Collection (findByElem)

newtype ScriptEvent = ScriptEvent Word32 deriving (Show, Eq)
    
newtype EventFlag = EventFlag Word32 deriving (Show, Eq)

noEventFlags :: EventFlag
noEventFlags = EventFlag 0

mkEventFlags :: Foldable t => t ScriptEvent -> EventFlag
mkEventFlags = F.foldl' (flip enableEventFlag) noEventFlags

enableEventFlag :: ScriptEvent -> EventFlag -> EventFlag
enableEventFlag (ScriptEvent e) (EventFlag f) = EventFlag $ f .|. e

disableEventFlag :: ScriptEvent -> EventFlag -> EventFlag
disableEventFlag (ScriptEvent e) (EventFlag f) = EventFlag $ f .&. complement e

setEventFlagEnabled :: ScriptEvent -> EventFlag -> Bool -> EventFlag
setEventFlagEnabled event flags True  = enableEventFlag event flags
setEventFlagEnabled event flags False = disableEventFlag event flags

isEventSet :: ScriptEvent -> EventFlag -> Bool
isEventSet (ScriptEvent e) (EventFlag f) = 0 /= e .&. f

onUpdate, onInit, onDestroy, onSignalChange, onSensorCollisionBegin, onSensorCollisionEnd :: ScriptEvent
onUpdate               = ScriptEvent $ bit 1
onInit                 = ScriptEvent $ bit 2
onDestroy              = ScriptEvent $ bit 3
onSignalChange         = ScriptEvent $ bit 4
onSensorCollisionBegin = ScriptEvent $ bit 5
onSensorCollisionEnd   = ScriptEvent $ bit 6

allEvents :: Map Text ScriptEvent
allEvents = Map.fromList
    [ ("on-update",                 onUpdate              )
    , ("on-init",                   onInit                )
    , ("on-destroy",                onDestroy             )
    , ("on-signal-change",          onSignalChange        )
    , ("on-sensor-collision-begin", onSensorCollisionBegin)
    , ("on-sensor-collision-end",   onSensorCollisionEnd  )
    ]

instance ToJSON ScriptEvent where
    toJSON se = toJSON textName
        where textName :: Maybe Text
              textName = fst <$> findByElem (==se) allEvents

instance FromJSON ScriptEvent where
    parseJSON = withText "ScriptEvent" $ \t -> case Map.lookup t allEvents of
        Just e  -> return e
        Nothing -> fail $ "Unkown ScriptEvent " ++ show t

instance ToJSON EventFlag where
    toJSON (EventFlag ef) = toJSON eventLs
        where eventLs = F.foldl' addIfSet [] . Map.assocs $ allEvents
              addIfSet acc (k, ScriptEvent se)
                  | ef .&. se /= 0 = k:acc
                  | otherwise      = acc

instance FromJSON EventFlag where
    parseJSON = withArray "EventFlag" $ \arr -> mkEventFlags <$> sequence (parseJSON <$> arr)
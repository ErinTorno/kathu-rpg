{-# LANGUAGE OverloadedStrings #-}

module Kathu.Scripting.Event
    ( ScriptEvent
    , EventFlag
    , noEventFlags
    , mkEventFlags
    , isEventSet
    , onUpdate
    , onInit
    , onDestroy
    ) where

import           Data.Aeson
import           Data.Bits
import           Data.Word
import qualified Data.Foldable    as F

newtype ScriptEvent = ScriptEvent Word32
    
newtype EventFlag = EventFlag Word32

noEventFlags :: EventFlag
noEventFlags = EventFlag 0

mkEventFlags :: Foldable t => t ScriptEvent -> EventFlag
mkEventFlags = F.foldl' (flip enableEventFlag) noEventFlags

enableEventFlag :: ScriptEvent -> EventFlag -> EventFlag
enableEventFlag (ScriptEvent e) (EventFlag f) = EventFlag $ f .|. e

isEventSet :: ScriptEvent -> EventFlag -> Bool
isEventSet (ScriptEvent e) (EventFlag f) = 0 /= e .&. f

onUpdate, onInit, onDestroy :: ScriptEvent
onUpdate  = ScriptEvent $ 2 ^ (0 :: Int)
onInit    = ScriptEvent $ 2 ^ (1 :: Int)
onDestroy = ScriptEvent $ 2 ^ (2 :: Int)

instance FromJSON ScriptEvent where
    parseJSON = withText "ScriptEvent" $ \txt -> case txt of
        "on-update"  -> pure onUpdate
        "on-init"    -> pure onInit
        "on-destroy" -> pure onDestroy
        e            -> fail $ "Unkown ScriptEvent " ++ show e

instance FromJSON EventFlag where
    parseJSON = withArray "EventFlag" $ \arr -> mkEventFlags <$> sequence (parseJSON <$> arr)
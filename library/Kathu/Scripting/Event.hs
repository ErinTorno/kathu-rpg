{-# LANGUAGE OverloadedStrings #-}

module Kathu.Scripting.Event
    ( ScriptEvent(..)
    , EventFlag
    , noEventFlags
    , mkEventFlags
    , isEventSet
    ) where

import           Data.Aeson
import           Data.Bits
import           Data.Word
import qualified Data.Foldable    as F

data ScriptEvent
    = OnUpdate
    
data EventFlag = EventFlag Word32

noEventFlags :: EventFlag
noEventFlags = EventFlag 0

mkEventFlags :: Foldable t => t ScriptEvent -> EventFlag
mkEventFlags = F.foldl' (flip enableEventFlag) noEventFlags

enableEventFlag :: ScriptEvent -> EventFlag -> EventFlag
enableEventFlag e (EventFlag f) = EventFlag . (f .|.) $ case e of
    OnUpdate -> 1

isEventSet :: ScriptEvent -> EventFlag -> Bool
isEventSet e (EventFlag f) = 0 /= case e of
    OnUpdate -> 1 .&. f

instance FromJSON ScriptEvent where
    parseJSON = withText "ScriptEvent" $ \txt -> case txt of
        "on-update" -> pure OnUpdate
        e           -> fail $ "Unkown ScriptEvent " ++ show e

instance FromJSON EventFlag where
    parseJSON = withArray "EventFlag" $ \arr -> mkEventFlags <$> sequence (parseJSON <$> arr)
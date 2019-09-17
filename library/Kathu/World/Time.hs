{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.World.Time where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Word
import GHC.Generics

import Kathu.Parsing.Aeson (standardProjectOptions)

-- our world time advances by one day for every (24 hours / timeScale) pass in the real world
timeScale :: Num a => a
timeScale = 30

newtype WorldTime = WorldTime Word64 deriving (Show, Eq, Generic)



data TimeOfDay = Dawn | Afternoon | Dusk | Night

instance ToJSON TimeOfDay where
    toJSON Dawn      = String "dawn"
    toJSON Afternoon = String "afternoon"
    toJSON Dusk      = String "dusk"
    toJSON Night     = String "night"
instance FromJSON TimeOfDay where
    parseJSON (String "dawn")      = pure Dawn
    parseJSON (String "afternoon") = pure Afternoon
    parseJSON (String "dusk")      = pure Dusk
    parseJSON (String "night")     = pure Night
    parseJSON (String s)           = error . concat $ ["Attempted to parse time of day with invalid time \"", show s, "\""]
    parseJSON v = typeMismatch "TimeOfDay" v



data DaylightConfig = DaylightConfig
    { timeOfDawn      :: Double
    , timeOfAfternoon :: Double
    , timeOfDusk      :: Double
    , timeOfNight     :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON DaylightConfig where
    toJSON = genericToJSON standardProjectOptions
instance FromJSON DaylightConfig where
    parseJSON = genericParseJSON standardProjectOptions


    
defaultDaylightConfig :: DaylightConfig
defaultDaylightConfig = DaylightConfig
    { timeOfDawn      = 6
    , timeOfAfternoon = 10
    , timeOfDusk      = 19
    , timeOfNight     = 21
    }

getTimeOfDay :: DaylightConfig -> WorldTime -> TimeOfDay
getTimeOfDay (DaylightConfig dawn noon dusk night) wt
    | h < dawn  = Night
    | h < noon  = Dawn
    | h < dusk  = Afternoon
    | h < night = Dusk
    | otherwise = Night
    where h = fromIntegral (getHour wt) + fromIntegral (getMinute wt) / 60.0

-- Get time units from WorldTime

getSecond :: WorldTime -> Int
getSecond (WorldTime t) = fromIntegral $ (t * timeScale) `quot` msPerSecond

getMinute :: WorldTime -> Int
getMinute (WorldTime t) = fromIntegral $ (t * timeScale) `quot` msPerMinute

getHour :: WorldTime -> Int
getHour (WorldTime t) = fromIntegral $ (t * timeScale) `quot` msPerHour

getDay :: WorldTime -> Int
getDay (WorldTime t) = fromIntegral $ (t * timeScale) `quot` msPerDay

getYear :: WorldTime -> Int
getYear (WorldTime t) = fromIntegral $ (t * timeScale) `quot` msPerYear

-- MS per time unit

msPerSecond :: Num a => a
msPerSecond = 1000

msPerMinute :: Num a => a
msPerMinute = 60 * msPerSecond

msPerHour :: Num a => a
msPerHour = 60 * msPerMinute

msPerDay :: Num a => a
msPerDay = 24 * msPerHour

msPerYear :: Num a => a
msPerYear = 365 * msPerDay
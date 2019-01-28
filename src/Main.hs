{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Apecs (runWith)
import Control.Monad (replicateM_, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import qualified SDL
import qualified SDL.Image as SDLI
import System.Environment

import Entity.System
import qualified Init
import Game (runGame)
import IO.Settings
import Render (runRender)
import qualified SDLCommon
import Util

import IO.File

appName = "Kathu"

updateDelay :: Word32
updateDelay = floor $ 1000.0 / 60.0 -- 60 ticks per second is ideal

-- determines the millisecond delay needed to hit goal fps. Will disregard fps goals beneath the physics delay
renderDelay :: Settings -> Word32
renderDelay = max updateDelay . floor . (1000.0/) . targetFPS

run :: Word32 -> SystemT' IO Bool -> SDL.Window -> Word32 -> Word32 -> SystemT' IO ()
run renderDelay b window prevPhysTime prevRendTime = b >>= go
    where go False = pure ()
          go True  = do
              startTime <- SDL.ticks
              let (n, remainder) = (startTime - prevPhysTime) `divMod` updateDelay

              -- physics steps as a constant rate as given by the update delay
              replicateM_ (fromIntegral n) $ do runGame updateDelay

              renderStartTime <- SDL.ticks
              let renderDiffer = renderStartTime - prevRendTime
              -- we delay unless physics took enough time that we should draw it again
              unless (renderDiffer >= renderDelay) $ SDL.delay (renderDelay - renderDiffer)
              -- render steps in variable time, so we must reflect that
              runRender window renderDiffer
              
              -- Physics steps back to ensure next update is on time; render goes whenever it can
              run renderDelay b window (startTime - remainder) renderStartTime

initialize :: Settings -> SDL.Window -> SystemT' IO ()
initialize settings window = do
    Init.system settings
    SDL.ticks >>= \t -> run (floor $ 1000.0 / (targetFPS settings)) (SDLCommon.isOpen <$> SDL.pollEvent) window t t

main :: IO ()
main = do settings <- loadSettings
          world    <- Init.entityWorld
          runWith world $ (SDLCommon.withSDL $ SDLCommon.withWindow appName (resolution settings) (initialize settings))
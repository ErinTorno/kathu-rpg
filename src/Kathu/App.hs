{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.App (start) where

import Apecs (runWith, lift)
import Control.Monad (replicateM_, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Kathu.Entity.System
import qualified Kathu.Init as Init
import Kathu.Game (runGame)
import Kathu.IO.Settings
import Kathu.Render (runRender, runRenderGL)
import qualified Kathu.SDLCommon as SDLC
import Kathu.Util
import qualified SDL
import SDL (($=))
import qualified SDL.Image as SDLI
import System.Environment

appName = "Kathu"

updateDelay :: Word32
updateDelay = floor $ 1000.0 / 60.0 -- 60 ticks per second is ideal

-- determines the millisecond delay needed to hit goal fps. Will disregard fps goals beneath the physics delay
renderDelay :: Settings -> Word32
renderDelay = max updateDelay . floor . (1000.0/) . targetFPS

run :: Word32 -> SystemT' IO Bool -> SDL.Window -> Word32 -> Word32 -> SystemT' IO ()
run renderDelay b !window !prevPhysTime !prevRendTime = b >>= go
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
              --lift runRenderGL
              
              -- Physics steps back to ensure next update is on time; render goes whenever it can
              run renderDelay b window (startTime - remainder) renderStartTime

initialize :: Settings -> SDL.Window -> SystemT' IO ()
initialize settings window = do
    Init.system settings
    context <- SDL.glCreateContext window
    (program, attrib) <- lift $ Init.openGL

    SDL.ticks >>= \t -> run (floor $ 1000.0 / (targetFPS settings)) (SDLC.isOpen <$> SDL.pollEvent) window t t

    SDL.glDeleteContext context
    SDL.destroyWindow window

start :: IO ()
start = do SDL.initialize [SDL.InitVideo]
           SDL.HintRenderScaleQuality $= SDL.ScaleNearest
           settings <- loadSettings
           world    <- Init.entityWorld
           runWith world $ (SDLC.withSDL $ SDLC.withWindow appName (resolution settings) (Just SDL.defaultOpenGL) (initialize settings))
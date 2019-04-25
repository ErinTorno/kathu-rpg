{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

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
import Kathu.Graphics.RenderBuffer (RenderBuffer, mkRenderBuffer)
import Kathu.IO.Settings
import Kathu.Render (runRender)
import qualified Kathu.SDLCommon as SDLC
import Kathu.Util
import qualified SDL
import SDL (($=))
import System.Environment

appName = "Kathu"

updateDelay :: Word32
updateDelay = floor $ 1000.0 / 60.0 -- 60 ticks per second is ideal

-- determines the millisecond delay needed to hit goal fps. Will disregard fps goals beneath the physics delay
renderDelay :: Settings -> Word32
renderDelay = max updateDelay . floor . (1000.0/) . targetFPS

run :: Word32 -> SystemT' IO Bool -> SDL.Window -> RenderBuffer -> Word32 -> Word32 -> SystemT' IO ()
run renderDelay b window renBuf !prevPhysTime !prevRendTime = b >>= go
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
              newRenBuf <- runRender window renBuf renderDiffer
              --lift runRenderGL
              
              -- Physics steps back to ensure next update is on time; render goes whenever it can
              run renderDelay b window newRenBuf (startTime - remainder) renderStartTime

start :: IO ()
start = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleNearest
    settings <- loadSettings
    world    <- Init.entityWorld
    let winConfig = SDL.defaultWindow
                        { SDL.windowInitialSize = SDL.V2 (fromIntegral . resolutionX $ settings) (fromIntegral . resolutionY $ settings)
                        , SDL.windowOpenGL = Nothing
                        }
    window   <- SDL.createWindow appName winConfig
    SDL.showWindow window
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    curTime      <- SDL.ticks
    renderBuffer <- mkRenderBuffer
    -- the main loop
    runWith world $
        Init.system renderer settings
        >> run (floor $ 1000.0 / (targetFPS settings)) (SDLC.isOpen <$> SDL.pollEvent) window renderBuffer curTime curTime

    -- dispose of resources
    SDL.destroyWindow window
    SDL.quit
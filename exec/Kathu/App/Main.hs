{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Kathu.App.Main (start) where

import Apecs (runWith)
import Control.Monad (replicateM_, unless)
import Data.Text (Text)
import Data.Word
import qualified SDL
import SDL (($=))

import Kathu.App.Data.Settings
import Kathu.App.Events
import Kathu.App.Graphics.Render (runRender)
import Kathu.App.Graphics.RenderBuffer (RenderBuffer, mkRenderBuffer)
import qualified Kathu.App.Init as Init
import Kathu.App.System
import Kathu.Game

appName :: Text
appName = "Kathu"

updateDelay :: Word32
updateDelay = floor $ 1000 / (60 :: Double) -- 60 ticks per second is ideal

-- determines the millisecond delay needed to hit goal fps. Will disregard fps goals beneath the physics delay
renderDelay :: Settings -> Word32
renderDelay = max updateDelay . floor . (1000/) . targetFPS

run :: Word32 -> SDL.Window -> RenderBuffer -> Word32 -> Word32 -> SystemT' IO ()
run renDelay window renBuf !prevPhysTime !prevRendTime = do
    startTime <- SDL.ticks
    let (n, remainder) = (startTime - prevPhysTime) `divMod` updateDelay

    -- physics steps as a constant rate as given by the update delay
    replicateM_ (fromIntegral n) $ handleControls >> runGame updateDelay
    shouldContinue <- runEvents

    renderStartTime <- SDL.ticks
    let renderDiffer = renderStartTime - prevRendTime
    -- we delay unless physics took enough time that we should draw it again
    unless (renderDiffer >= renDelay) $ SDL.delay (renDelay - renderDiffer)
    -- render steps in variable time, so we must reflect that
    newRenBuf <- runRender window renBuf renderDiffer
    
    -- Physics steps back to ensure next update is on time; render goes whenever it can
    if not shouldContinue then
        pure ()
    else
        run renDelay window newRenBuf (startTime - remainder) renderStartTime

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

    curTime      <- SDL.ticks
    renderBuffer <- mkRenderBuffer
    -- the main loop
    runWith world $
        Init.system settings
        >> run (renderDelay settings) window renderBuffer curTime curTime

    -- dispose of resources
    SDL.destroyWindow window
    SDL.quit
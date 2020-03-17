{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Kathu.App.Main (start, startWith) where

import Apecs (runWith)
import Apecs.Physics (stepPhysics)
import Control.Monad (replicateM_, unless)
import Data.Text (Text)
import Data.Word
import qualified SDL
import qualified SDL.Font as SDLF
import SDL (($=))

import Kathu.App.Data.Settings
import Kathu.App.Events
import Kathu.App.Graphics.Render (runRender)
import Kathu.App.Graphics.RenderBuffer (RenderBuffer, mkRenderBuffer)
import qualified Kathu.App.Init as Init
import Kathu.App.System
import Kathu.Game (runGame, updateDelay)

appName :: Text
appName = "Kathu"

-- determines the millisecond delay needed to hit goal fps. Will disregard fps goals beneath the physics delay
renderDelay :: Settings -> Word32
renderDelay = max updateDelay . floor . (1000/) . targetFPS

run :: Word32 -> SDL.Renderer -> RenderBuffer -> Word32 -> Word32 -> SystemT' IO ()
run renDelay renderer renBuf !prevPhysTime !prevRendTime = do
    startTime <- SDL.ticks
    let (n, remainder) = (startTime - prevPhysTime) `divMod` updateDelay

    -- physics steps as a constant rate as given by the update delay
    replicateM_ (fromIntegral n) $ handleControls
                                >> runGame destroyEntity updateDelay
                                >> stepPhysics (fromIntegral  updateDelay / 1000)
    shouldContinue <- runEvents

    renderStartTime <- SDL.ticks
    let renderDiffer = renderStartTime - prevRendTime
    -- we delay unless physics took enough time that we should draw it again
    unless (renderDiffer >= renDelay) $ SDL.delay (renDelay - renderDiffer)
    -- render steps in variable time, so we must reflect that
    runRender renderer renBuf renderDiffer
    
    -- Physics steps back to ensure next update is on time; render goes whenever it can
    if not shouldContinue then
        pure ()
    else
        run renDelay renderer renBuf (startTime - remainder) renderStartTime

start :: IO ()
start = startWith $ \settings -> let config = SDL.defaultWindow { SDL.windowInitialSize = fromIntegral <$> resolution settings }
                                  in SDL.createWindow appName config

startWith :: (Settings -> IO SDL.Window) -> IO ()
startWith createWindow = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleNearest
    settings <- loadSettings
    world    <- Init.entityWorld
    window   <- createWindow settings
    SDL.showWindow window
    SDLF.initialize
    renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig (if isVSyncEnabled settings then SDL.AcceleratedVSyncRenderer else SDL.AcceleratedRenderer) False

    curTime      <- SDL.ticks
    renderBuffer <- mkRenderBuffer
    -- the main loop
    runWith world $
        Init.system window renderer settings
        >> run (renderDelay settings) renderer renderBuffer curTime curTime

    -- dispose of resources
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDLF.quit
    SDL.quit
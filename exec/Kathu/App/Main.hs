{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Kathu.App.Main
    ( RenderInfo(..)
    , appName
    , renderDelay
    , start
    , startWith
    , run
    , runForEventQueue) where

import           Apecs                           hiding (($=))
import           Apecs.Physics                   (stepPhysics)
import           Control.Monad                   (replicateM_, when, unless)
import           Data.Text                       (Text)
import           Data.Word
import qualified SDL
import qualified SDL.Font                        as SDLF
import           SDL                             (($=))

import           Kathu.App.Data.Settings
import           Kathu.App.Events
import           Kathu.App.Graphics.Render       (runRender)
import           Kathu.App.Graphics.RenderBuffer (RenderBuffer, mkRenderBuffer)
import           Kathu.App.Tools.Commands
import           Kathu.App.Tools.EventQueue
import           Kathu.App.Tools.ToolMode
import qualified Kathu.App.Init                  as Init
import           Kathu.App.System
import           Kathu.Game                      (runGame, updateDelay)

appName :: Text
appName = "Kathu"

data RenderInfo = RenderInfo {sdlWindow :: SDL.Window, sdlRenderer :: SDL.Renderer, renderBuffer :: RenderBuffer, gameSettings :: Settings}

-- determines the millisecond delay needed to hit goal fps. Will disregard fps goals beneath the physics delay
renderDelay :: Settings -> Word32
renderDelay = max updateDelay . floor . (1000/) . targetFPS

createWindow :: IO RenderInfo
createWindow = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleNearest
    settings <- loadSettings

    let config = SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> resolution settings}

    window   <- SDL.createWindow appName config
    SDL.showWindow window
    SDLF.initialize
    renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig (if isVSyncEnabled settings then SDL.AcceleratedVSyncRenderer else SDL.AcceleratedRenderer) False
    buffer   <- mkRenderBuffer
    pure $ RenderInfo window renderer buffer settings

createWorld :: RenderInfo -> IO EntityWorld
createWorld (RenderInfo window renderer _ settings) = do
    world <- Init.entityWorld

    runWith world $
        Init.system window renderer settings

    pure world

destroyWindow :: RenderInfo -> IO ()
destroyWindow RenderInfo {sdlWindow = window, sdlRenderer = renderer} = do
    -- dispose of resources
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDLF.quit
    SDL.quit

start :: IO ()
start = startWith id $ \(RenderInfo _ renderer buffer settings) world -> do
    curTime      <- SDL.ticks
    -- the main loop
    runWith world $
        run (renderDelay settings) renderer buffer curTime curTime

startWith :: (Settings -> Settings) -> (RenderInfo -> EntityWorld -> IO ()) -> IO ()
startWith updateSettings runner = do
    renInfo      <- createWindow
    world        <- createWorld $ renInfo {gameSettings = updateSettings $ gameSettings renInfo}
    runner renInfo world

    destroyWindow renInfo
    SDL.quit

replicateRunGame :: Integral a => a -> SystemT' IO ()
replicateRunGame n = replicateM_ (fromIntegral n) $ do
    runEvents
    runGame destroyEntity updateDelay
    stepPhysics (fromIntegral  updateDelay / 1000)

run :: Word32 -> SDL.Renderer -> RenderBuffer -> Word32 -> Word32 -> SystemT' IO ()
run renDelay renderer renBuf !prevPhysTime !prevRendTime = do
    startTime <- SDL.ticks
    let (n, remainder) = (startTime - prevPhysTime) `divMod` updateDelay

    -- physics steps as a constant rate as given by the update delay
    replicateRunGame n

    renderStartTime <- SDL.ticks
    let renderDiffer = renderStartTime - prevRendTime
    -- we delay unless physics took enough time that we should draw it again
    unless (renderDiffer >= renDelay) $
        SDL.delay (renDelay - renderDiffer)
    -- render steps in variable time, so we must reflect that
    runRender renderer renBuf renderDiffer
    
    ShouldQuit isQuitting <- get global
    -- Physics steps back to ensure next update is on time; render goes whenever it can
    if isQuitting
    then pure ()
    else run renDelay renderer renBuf (startTime - remainder) renderStartTime

runForEventQueue :: EventQueue -> CommandState -> Word32 -> SDL.Renderer -> RenderBuffer -> Word32 -> Word32 -> IO ()
runForEventQueue queue commandState renDelay renderer renBuf !prevPhysTime !prevRendTime = do
    -- the editor might want to work with the world, so we need to make sure it hasn't taken the world when this runs
    world         <- takeEntityWorld queue
    shouldRunGame <- runWith world $ do
        handleEvents queue commandState
        not . usesFreeCam <$> get global

    startTime <- SDL.ticks
    let (n, remainder) = (startTime - prevPhysTime) `divMod` updateDelay

    when shouldRunGame $ runWith world (replicateRunGame n)
    runWith world runEvents

    renderStartTime <- SDL.ticks
    let renderDiffer = renderStartTime - prevRendTime

    -- we might want to delay until ready to run again; in this case we should yield the world until we are done delaying
    unless (renderDiffer >= renDelay) $
        SDL.delay (renDelay - renderDiffer)
    runWith world $ runRender renderer renBuf renderDiffer
    
    putEntityWorld world queue
    
    ShouldQuit isQuitting <- runWith world $ get global
    -- we ignore close events; only the editor should be closed, and when it closes this should close too
    when isQuitting $
        pushEditorEvent queue TryToCloseEditor
    runForEventQueue queue commandState renDelay renderer renBuf (startTime - remainder) renderStartTime
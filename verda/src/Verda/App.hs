{-# LANGUAGE RecordWildCards #-}

module Verda.App
    ( AppConfig(..)
    , run
    ) where

import           Apecs
import           Control.Concurrent.MVar
import           Control.Monad               (replicateM_, unless, when)
import           Data.Text                   (Text)
import           Data.Word
import           Linear.V2
import qualified SDL
import qualified SDL.Font                    as SDLF
import qualified SDL.Mixer                   as SDLM

import           Verda.Graphics.Components
import           Verda.Graphics.Icons
import           Verda.Graphics.Renderer
import           Verda.Graphics.SpriteBuffer
import           Verda.Event.EventHandler    (handleEvents)
import           Verda.Time
import           Verda.World

data AppConfig w = AppConfig
    { appName            :: !Text
    , resolution         :: !(V2 Int)
    , updateHertz        :: !Double
    , renderHertz        :: !Double
    , appIcon            :: !(IO Icon)
    , appWorld           :: !(IO w)
    , initWorld          :: !(SDL.Window -> SDL.Renderer -> SystemT w IO ())
    , runGame            :: !(Word32 -> SystemT w IO ())
    , concurrentWorldVar :: !(SystemT w IO (Maybe (MVar w))) -- | When just, game loop uses the world in this MVar to support multi-threaded access of it with other windows
    }

run :: VerdaWorld w IO => AppConfig w -> IO ()
run AppConfig{..} = do
    liftIO (SDL.HintRenderDriver SDL.$= SDL.OpenGL)
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear

    let config = SDL.defaultWindow { SDL.windowInitialSize     = fromIntegral <$> resolution
                                   , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL }
    window <- SDL.createWindow appName config
    SDL.showWindow window
    SDLF.initialize
    renderer <- SDL.createRenderer window (-1) $ SDL.defaultRenderer {SDL.rendererTargetTexture = True}

    _ <- SDL.glCreateContext window

    let updateDelay = floor . (1000/) $ updateHertz
        renderDelay = max updateDelay . floor . (1000/) $ renderHertz

    setWindowIcon window =<< appIcon
    screenTex    <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (fromIntegral <$> resolution)
    world        <- appWorld
    spritebuffer <- mkSpriteBuffer
    SDLM.withAudio SDLM.defaultAudio 4096 $
        runWith world $ do
            initVerdaWorld
            initWorld window renderer
            global $= Resolution resolution
            curTime <- lift SDL.ticks
            maybeMVar <- concurrentWorldVar
            case maybeMVar of
                Just mVar -> lift $ runGameConcurrentLoop window renderer screenTex spritebuffer runGame updateDelay renderDelay (curTime - updateDelay) (curTime - renderDelay) mVar
                Nothing   -> runGameLoop window renderer screenTex spritebuffer runGame updateDelay renderDelay (curTime - updateDelay) (curTime - renderDelay)

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDLF.quit
    SDL.quit

runGameLoop :: VerdaWorld w IO => SDL.Window -> SDL.Renderer -> SDL.Texture -> SpriteBuffer -> (Word32 -> SystemT w IO ()) -> Word32 -> Word32 -> Word32 -> Word32 -> SystemT w IO ()
runGameLoop !window !renderer !screenTex !renBuf !runner !updateDelay !renDelay !lastUpdateTime !lastRenTime = do
    startTime <- SDL.ticks
    let (n, remainder) = (startTime - lastUpdateTime) `divMod` updateDelay
    logicStep (fromIntegral n) updateDelay runner

    renderStartTime <- SDL.ticks
    let renderDiffer = renderStartTime - lastRenTime
    -- we delay unless physics took enough time that we should draw it again
    unless (renderDiffer >= renDelay) $
        SDL.delay (renDelay - renderDiffer)
    -- render steps in variable time, so we must reflect that
    runRender window renderer screenTex renBuf renderDiffer

    runState <- get global
    unless (runState == Quitting) $
        runGameLoop window renderer screenTex renBuf runner updateDelay renDelay (startTime - remainder) renderStartTime

runGameConcurrentLoop :: VerdaWorld w IO => SDL.Window -> SDL.Renderer -> SDL.Texture -> SpriteBuffer -> (Word32 -> SystemT w IO ()) -> Word32 -> Word32 -> Word32 -> Word32 -> MVar w -> IO ()
runGameConcurrentLoop !window !renderer !screenTex !renBuf !runner !updateDelay !renDelay !lastUpdateTime !lastRenTime !worldMVar = do
    startTime <- SDL.ticks
    let (n, remainder) = (startTime - lastUpdateTime) `divMod` updateDelay
    world <- takeMVar worldMVar
    runWith world $ logicStep (fromIntegral n) updateDelay runner

    renderStartTime <- SDL.ticks
    let renderDiffer = renderStartTime - lastRenTime
    -- we delay unless physics took enough time that we should draw it again
    unless (renderDiffer >= renDelay) $
        SDL.delay (renDelay - renderDiffer)
    runWith world $
        -- render steps in variable time, so we must reflect that
        runRender window renderer screenTex renBuf renderDiffer

    runState <- runWith world $ get global
    putMVar worldMVar world
    unless (runState == Quitting) $
        runGameConcurrentLoop window renderer screenTex renBuf runner updateDelay renDelay (startTime - remainder) renderStartTime worldMVar

logicStep :: VerdaWorld w IO => Int -> Word32 -> (Word32 -> SystemT w IO ()) -> SystemT w IO ()
logicStep n updateDelay runner = replicateM_ n $ do
    -- physics steps as a constant rate as given by the update delay
    stepLogicTime updateDelay
    handleEvents
    runState <- get global
    when (runState == Running)
        updateVerdaWorld
    runner updateDelay
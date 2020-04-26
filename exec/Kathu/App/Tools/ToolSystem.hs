{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kathu.App.Tools.ToolSystem where

import           Apecs
import           Apecs.Physics
import           Control.Lens
import           Control.Monad               (forM_, when)
import           Linear.V2                   (V2(..), _x, _y)
import qualified SDL

import           Kathu.App.Data.Controls
import           Kathu.App.Data.Library
import           Kathu.App.Data.Settings
import           Kathu.App.Graphics.Drawing
import           Kathu.App.System
import           Kathu.App.Tools.ToolMode
import           Kathu.Entity.Components     (Local, simpleIdentity)
import           Kathu.Entity.Cursor
import           Kathu.Entity.Logger
import           Kathu.Graphics.Camera
import           Kathu.Graphics.Color
import           Kathu.Util.Apecs
import           Kathu.Util.Flow             (ireplicateM_)

gridColor :: Color
gridColor = mkColor 180 134 134 150

renderToolMode :: SDL.Renderer -> (V2 Double -> V2 Double) -> SystemT' IO ()
renderToolMode renderer logicToRenderPos = do
    toolmode <- get global
    when (shouldShowGrid toolmode) $ do
        V2 resW resH <- fmap fromIntegral . resolution <$> get global
        cam          <- getUnique
        forM_ cam $ \(Camera camZoom, Position (V2 camX camY)) -> do
            let unitHeightToDraw, unitWidthToDraw, aspect :: Double
                unitHeightToDraw = maxUnitsPerHeight * camZoom + 2
                unitWidthToDraw  = unitHeightToDraw * aspect
                aspect           = fromIntegral resW / fromIntegral resH
                floorF d         = fromIntegral (floor d :: Int)

            SDL.rendererDrawColor renderer SDL.$= unColor gridColor
            -- draws horizontal grid lines
            ireplicateM_ (floor unitHeightToDraw) $ \row ->
                let rowScreenCoord = floor . view _y . logicToRenderPos . V2 0 . floorF $ (camY - 0.5 * unitHeightToDraw + fromIntegral row)
                 in SDL.drawLine renderer (SDL.P (V2 0 rowScreenCoord)) (SDL.P (V2 resW rowScreenCoord))
            -- draws vertical grid lines
            ireplicateM_ (floor unitWidthToDraw) $ \col ->
                --  add 0.5 after flooring as tiles are normally centered, and we want to offset that
                let colScreenCoord = floor . view _x . logicToRenderPos . flip V2 0 . (+0.5) . floorF $ (camX - 0.5 * unitWidthToDraw + fromIntegral col)
                 in SDL.drawLine renderer (SDL.P (V2 colScreenCoord 0)) (SDL.P (V2 colScreenCoord resH))

-- | Updates tool mode after new frame's controls have been updated
updateToolMode :: SystemT' IO ()
updateToolMode = do
    cursorSt  <- get global
    toolmode  <- get global
    when (usesFreeCam toolmode) $ do
        controlSt <- get global

        middleMouseSt <- getInputState controlSt (fromMouseButton SDL.ButtonMiddle)
        when (isPressedOrHeld middleMouseSt) $
            cmap $ \(_ :: Camera, Position pos) ->
                Position $ pos - cursorMovement cursorSt

        let scroll = cursorScrollWheel cursorSt
        when (scroll /= 0) $
            cmap $ \(Camera z) ->
                Camera $ max 0 (z - 0.1 * scroll)
    case toolmode of
        TilePlacer TilePlacerState{tileSelectorEty = selectorEty} -> do
            let hoveredTilePos = fromIntegral <$> ((floor <$> V2 0.5 1 + cursorPosition cursorSt) :: V2 Int)
            -- we move the tile selector entity to hover over the currently selected tile
            forM_ selectorEty $ \ety ->
                ety $= Position hoveredTilePos
        _ -> pure ()

-- | Handles changing toolmodes after the editor has pushed the UseToolMode event
handleUseToolModeEvent :: ToolMode -> SystemT' IO ()
handleUseToolModeEvent newMode = do
    prevMode <- get global
    global   $= newMode
    -- the new mode is a free cam mode, but not the last
    -- let's create a new free cam entity, and give it the camera
    when (usesFreeCam newMode && not (usesFreeCam prevMode)) $ do
        playerEty  <- getUnique
        cmap $ \(_ :: Camera) -> Not :: Not Camera

        freeCamEty <- newEntity (simpleIdentity "ToolMode Free Camera", DynamicBody)
        case playerEty of
            Just (cam :: Camera, p :: Position) -> freeCamEty $= (cam, p)
            Nothing                             -> freeCamEty $= (defaultCamera, Position (V2 0 0))
    -- the new mode is not a free cam mode, but the last is
    -- let's destroy the free cam entity, and give the camera back to the player
    when (usesFreeCam prevMode && not (usesFreeCam newMode)) $ do
        lastCamEty <- getUnique
        playerEty  <- getUnique
        -- Removes camera, then destroys free cam entity
        forM_ lastCamEty $ \(_ :: Camera, ety) -> do
            cmap $ \(_ :: Camera) -> Not :: Not Camera
            destroyEntity ety
        -- Gives the camera to the player
        forM_ playerEty $ \(_ :: Local, ety) ->
            ety $= defaultCamera

    finalizeToolMode prevMode
    initToolMode newMode

-- Warning, after calling it is unsafe to make any more uses of these tool mode states
finalizeToolMode :: ToolMode -> SystemT' IO ()
finalizeToolMode mode = case mode of
    TilePlacer TilePlacerState{tileSelectorEty = maybeSelectorEty} ->
        -- we don't want a tile selector anymore
        forM_ maybeSelectorEty $ \ety -> destroyEntity ety
    _ -> pure ()

initToolMode :: ToolMode -> SystemT' IO ()
initToolMode mode = case mode of
    TilePlacer st -> do
        library <- get global
        -- we want to create a tile placer entity to help show which tiles are which
        case lookupFromLibrary library prototypes "editor-tile-selector" of
            Nothing    -> logLine Warning "Entity config editor-tile-selector was not loaded"
            Just proto -> do
                ety    <- newFromPrototype proto
                ety    $= Position (V2 10000 10000) -- should be offscreen for first frame before it get's updated to follow camera
                global $= TilePlacer st{tileSelectorEty = Just ety}
    _ -> pure ()
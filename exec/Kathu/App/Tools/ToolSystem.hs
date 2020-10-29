{-# LANGUAGE UnboxedTuples #-}

module Kathu.App.Tools.ToolSystem
    ( addToolSystemExtension
    , runToolMode
    , handleUseToolModeEvent
    , rebuildEntityInfoCollisions
    ) where

import           Apecs
import           Apecs.Physics
import           Control.Lens
import           Control.Monad               (forM_, void, unless, when)
import           Data.Maybe                  (fromJust, isJust)
import           Data.Word
import qualified SDL
import           Verda.Event.Controls
import           Verda.Graphics.Components   (Camera(..), defaultCamera)
import           Verda.Graphics.Drawing
import           Verda.Graphics.Sprites      (Sprite, spriteRectangle)
import           Verda.Logger
import           Verda.System.Tile.Chunks
import           Verda.System.Tile.Components (tsTileID)
import           Verda.Time
import           Verda.Util.Flow             (ireplicateM_)
import           Verda.Util.Apecs
import           Verda.World                 (addRendererExtension)

import           Kathu.App.System
import           Kathu.App.Tools.Commands
import           Kathu.App.Tools.EventQueue
import           Kathu.App.Tools.ToolMode
import           Kathu.App.World
import           Kathu.Config.Dictionary
import           Kathu.Config.Settings
import           Kathu.Entity.Components
import           Kathu.Entity.Physics.CollisionGroup
import           Verda.Graphics.Color
import           Kathu.World.Tile
import           Kathu.World.WorldSpace

gridColor :: Color
gridColor = mkColor 100 80 100 255

placeLineColor :: Color
placeLineColor = mkColor 150 120 150 255

timeBetweenUndoRedo :: Word32
timeBetweenUndoRedo = 200 -- ms

addToolSystemExtension :: SystemT' IO ()
addToolSystemExtension = addRendererExtension $ \renderer logicToRenderPos _ -> do
    toolmode <- get global
    when (shouldShowGrid toolmode) $ do
        V2 resW resH <- fmap fromIntegral . resolution <$> get global
        cam          <- getUnique
        forM_ cam $ \(Camera camZoom, Position (V2 camX camY)) -> do
            let unitHeightToDraw, unitWidthToDraw, aspect :: Double
                unitHeightToDraw = maxUnitsPerHeight * camZoom + 2
                unitWidthToDraw  = unitHeightToDraw * aspect
                aspect           = fromIntegral resW / fromIntegral resH
                floorF           = fromIntegral @Int . floor

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
    case toolmode of
        TilePlacer _ -> do
            controlSt <- get global
            shiftSt   <- getInputState controlSt $ fromScanCode SDL.ScancodeLShift
            when (isPressedOrHeld shiftSt) $ do
                cursorSt         <- get global
                maybeLastTilePos <- lastPlacedTilePos <$> get global

                forM_ maybeLastTilePos $ \lastPos ->
                    let hoveredTilePos :: V2 Int
                        hoveredTilePos = floor <$> V2 0.5 1 + cursorPosition cursorSt
                        -- we want to show lines originating from the center of tiles, so we adjust them
                        shiftPos       = V2 0 (-0.5)
                        mkPoint pos    = SDL.P . fmap floor . logicToRenderPos $ shiftPos + (fromIntegral <$> pos)
                     in do SDL.rendererDrawColor renderer SDL.$= unColor placeLineColor
                           SDL.drawLine renderer (mkPoint lastPos) (mkPoint hoveredTilePos)
        _ -> pure ()

-- | Updates tool mode after new frame's controls have been updated
runToolMode :: EventQueue -> CommandState -> SystemT' IO ()
runToolMode queue commandSt = do
    cursorSt  <- get global
    toolmode  <- get global
    controlSt <- get global
    RenderTime time <- get global
    when (usesFreeCam toolmode) $ do
        middleMouseSt <- getInputState controlSt (fromMouseButton SDL.ButtonMiddle)
        when (isPressedOrHeld middleMouseSt) $
            cmap $ \(_ :: Camera, Position pos) ->
                Position $ pos - cursorMovement cursorSt

        let scroll = cursorScrollWheel cursorSt
        when (scroll /= 0) $
            cmap $ \(Camera z) ->
                Camera $ max 0 (z - 0.1 * scroll)

    -- handles right-clicking to open an entity instance editor window
    when (canEditEntities toolmode) $ do
        rightClickSt <- getInputState controlSt $ fromMouseButton SDL.ButtonRight
        univToolSt   <- get global
        when (isPressedOrHeld rightClickSt && canEditEntityInstance univToolSt) $ do
            maybeQueryRes <- pointQuery (cursorPosition cursorSt) 0.1 editorSensorFilter
            forM_ maybeQueryRes $ \PointQueryResult{pqShape = ety} -> do
                -- must exist, as we collided with its shape
                Shape parentEty _ <- get ety
                maybeProto <- getIfExists parentEty
                case maybeProto of
                    Nothing ->
                        logLine Warning "Attempted to edit entity that was in the editorSensorFilter group, but was missing the EditorInstancedFromWorld component"
                    Just (EditorInstancedFromWorld instancedProto) -> do
                        -- update the last time since we successfully chose an entity to edit
                        global $= univToolSt {canEditEntityInstance = False}
                        lift $ pushEditorEvent queue (EditEntityInstance parentEty instancedProto)
    
    unless (isNoTool toolmode) $ do
        univToolSt <- get global
        ctrlSt <- getInputState controlSt $ fromScanCode SDL.ScancodeLCtrl
        zSt    <- getInputState controlSt $ fromScanCode SDL.ScancodeZ
        ySt    <- getInputState controlSt $ fromScanCode SDL.ScancodeY

        when (isPressedOrHeld ctrlSt && lastUndoRedoTime univToolSt + timeBetweenUndoRedo <= time) $
            if | isPressedOrHeld zSt -> do
                   undoLastCommand commandSt
                   global $= univToolSt {lastUndoRedoTime = time}
               | isPressedOrHeld ySt -> do
                    redoNextCommand commandSt
                    global $= univToolSt {lastUndoRedoTime = time}
               | otherwise ->
                   pure ()

        case toolmode of
            TilePlacer TilePlacerState{tileSelectorEty = selectorEty} -> do
                let hoveredTilePos = floor <$> V2 0.5 1 + cursorPosition cursorSt
                    sTile          = selectedTile univToolSt
                -- we move the tile selector entity to hover over the currently selected tile
                forM_ selectorEty $ \ety ->
                    ety $= Position (fromIntegral <$> hoveredTilePos)

                runTilePlaceCommand commandSt sTile hoveredTilePos
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
            
    when (canEditEntities newMode && not (canEditEntities prevMode)) $
        buildEntityInfoCollisions

    when (canEditEntities prevMode && not (canEditEntities newMode)) $
        destroyEntityInfoCollisions

    unless (newMode `isSameMode` prevMode) $ do
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
    NoTool -> do
        -- these might have changed when we could run commands
        rebuildCurrentTileCollisions
        -- delete all editor references
        let cleanUpRef (EditorRefTo _, ety) = destroyEntity ety
            cleanUpRef (_, _)               = pure ()
        cmapM_ cleanUpRef
    TilePlacer st -> do
        dictionary <- get global
        -- we want to create a tile placer entity to help show which tiles are which
        case dictionaryLookup dictionary dictPrefabs "editor-tile-selector" of
            Nothing    -> logLine Warning "Entity config editor-tile-selector was not loaded"
            Just prefab -> do
                ety    <- newFromPrefab prefab
                ety    $= Position (V2 10000 10000) -- should be offscreen for first frame before it gets updated to follow camera
                global $= TilePlacer st {tileSelectorEty = Just ety}
    _ -> pure ()

--------------------
-- Entity Editing --
--------------------

rebuildEntityInfoCollisions :: SystemT' IO ()
rebuildEntityInfoCollisions = do
    destroyEntityInfoCollisions
    buildEntityInfoCollisions

destroyEntityInfoCollisions :: SystemT' IO ()
destroyEntityInfoCollisions =
    cmapM_ $ \(colFil, ety) ->
        when (colFil == editorInfoFilter) $
            destroyEntity ety

buildEntityInfoCollisions :: SystemT' IO ()
buildEntityInfoCollisions = do
    cmapM_ $ \(sprite :: Sprite, _ :: EditorInstancedFromWorld, ety) ->
        let (V2 mx my) = (/pixelsPerUnit) . fromIntegral <$> (let (SDL.Rectangle _ v) = spriteRectangle sprite in v)
            -- sprites are drawn centered above the logical coordinate, so we shift this new shape too
            convex     = oRectangle (V2 (-(mx / 2)) (-my)) (V2 mx my)
         in void $ newExistingEntity (Shape ety convex, editorInfoFilter, EditorRefTo ety)

----------------
-- TilePlacer --
----------------

runTilePlaceCommand :: CommandState -> Tile -> V2 Int -> SystemT' IO ()
runTilePlaceCommand commandSt sTile hoveredTilePos = do
    controlSt    <- get global
    shiftSt      <- getInputState controlSt $ fromScanCode SDL.ScancodeLShift
    leftMouseSt  <- getInputState controlSt $ fromMouseButton SDL.ButtonLeft
    maybeLastPos <- lastPlacedTilePos <$> get global
    
    when (isPressedOrHeld leftMouseSt) $
        if   isPressedOrHeld shiftSt && isJust maybeLastPos && fromJust maybeLastPos /= hoveredTilePos
        then do
            command <- mkLineTilePlaceCommand (fromJust maybeLastPos) hoveredTilePos sTile
            runCommand commandSt command
        else do
            prevTileSt <- getChunksTileState hoveredTilePos
            let isClick  = leftMouseSt == BtnPressed
                diffTile = prevTileSt^.tsTileID /= sTile^.tileID
                diffPos  = maybeLastPos /= Just hoveredTilePos
            -- only generate a new command if we click again, the tile is different from the square, or the position is different from last time
            when (isClick || diffTile || diffPos) $ do
                command <- mkSingleTilePlaceCommand hoveredTilePos sTile prevTileSt
                runCommand commandSt command

mkSingleTilePlaceCommand :: V2 Int -> Tile -> TileState -> SystemT' IO Command
mkSingleTilePlaceCommand hoveredTilePos sTile prevTileSt = do
    maybeLastPos <- lastPlacedTilePos <$> get global

    pure $ Command
        { applyCommand = do
            setChunksTileState hoveredTilePos (mkTileState sTile)
            univToolSt <- get global
            global     $= univToolSt {lastPlacedTilePos = Just hoveredTilePos}
        , removeCommand = do
            setChunksTileState hoveredTilePos prevTileSt
            univToolSt <- get global
            global     $= univToolSt {lastPlacedTilePos = maybeLastPos}
        }

mkLineTilePlaceCommand :: V2 Int -> V2 Int -> Tile -> SystemT' IO Command
mkLineTilePlaceCommand lastPos hoveredTilePos sTile = do
    prevTileSt <- replicateLineM lastPos hoveredTilePos (\pos -> (,pos) <$> getChunksTileState pos)

    pure $ Command 
        { applyCommand = do
            void $ replicateLineM lastPos hoveredTilePos (flip setChunksTileState $ mkTileState sTile)
            univToolSt <- get global
            global     $= univToolSt {lastPlacedTilePos = Just hoveredTilePos}
        , removeCommand = do
            forM_ prevTileSt $ uncurry (flip setChunksTileState)
            univToolSt <- get global
            global     $= univToolSt {lastPlacedTilePos = Just lastPos}
        }

-- Bresenham's line algorithm
replicateLineM :: Monad m => V2 Int -> V2 Int -> (V2 Int -> m a) -> m [a]
replicateLineM (V2 !x0 !y0) (V2 !x1 !y1) action
    | abs (y1 - y0) < abs (x1 - x0) = chooseSolver replicateLineLowM (x0 > x1)
    | otherwise                     = chooseSolver replicateLineHighM (y0 > y1)
    where chooseSolver runner cond
              | cond      = runner x1 y1 x0 y0 action
              | otherwise = runner x0 y0 x1 y1 action

replicateLineLowM :: Monad m => Int -> Int -> Int -> Int -> (V2 Int -> m a) -> m [a]
replicateLineLowM x0 y0 x1 y1 action = go [] x0 y0 (2 * dy - dx)
    where dx    = x1 - x0
          dy    = if y1 - y0 < 0 then y0 - y1 else y1 - y0
          yIncr = if y1 - y0 < 0 then -1 else 1
          go acc x y d
              | x > x1    = pure acc
              | otherwise = action (V2 x y)
                        >>= \a -> if d > 0
                                  then go (a:acc) (x + 1) (y + yIncr) ((d - 2 * dx) + 2 * dy)
                                  else go (a:acc) (x + 1) y (d + 2 * dy)

replicateLineHighM :: Monad m => Int -> Int -> Int -> Int -> (V2 Int -> m a) -> m [a]
replicateLineHighM x0 y0 x1 y1 action = go [] x0 y0 (2 * dx - dy)
    where dx    = if x1 - x0 < 0 then x0 - x1 else x1 - x0
          dy    = y1 - y0
          xIncr = if x1 - x0 < 0 then -1 else 1
          go acc x y d
              | y > y1    = pure acc
              | otherwise = action (V2 x y)
                        >>= \a -> if d > 0
                                  then go (a:acc) (x + xIncr) (y + 1) ((d - 2 * dy) + 2 * dx)
                                  else go (a:acc) x (y + 1) (d + 2 * dx)
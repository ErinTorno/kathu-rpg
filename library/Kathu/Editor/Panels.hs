{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Panels where

import qualified Apecs
import           Control.Lens               hiding (set)
import           Control.Monad              (forM_, void, when)
import           Data.GI.Base
import           Data.IORef
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vec
import qualified GI.Gtk                     as Gtk
import qualified GI.GdkPixbuf               as Gdk
import           Verda.Graphics.Sprites     (SpriteID(..), spriteID)

import           Kathu.Editor.Tools.EventQueue
import           Kathu.Editor.Tools.ToolMode
import           Kathu.Config.Dictionary   (dictParsingStore, dictTiles)
import           Kathu.Config.KathuStore   (psCountingIDs)
import           Kathu.Editor.Dialogs.ScriptEditor
import           Kathu.Editor.Resources
import           Kathu.Editor.Types
import           Kathu.Editor.Util.PropertyGrid
import           Verda.Parsing.Counting
import qualified Kathu.Scripting.Lua        as Lua
import           Verda.Util.Containers      (fromJustElseError)
import           Kathu.World.Tile           (TileRenderMode(..), emptyTile, emptyTileID, tileID, tileName, tileRenderMode)
import           Kathu.World.WorldSpace

mkTileIcon :: Text -> IO Gtk.Image
mkTileIcon path = do
    fullImg <- Gdk.pixbufNewFromFile (T.unpack path)
    width   <- Gdk.pixbufGetWidth  fullImg
    height  <- Gdk.pixbufGetHeight fullImg

    let fromJustElseMemError = fromJustElseError "No more memory could be allocated for Tile Pixbuf"

    croppedPixBuf <- if
        | width  < 16 || height  < 16 -> error $ "Tile icon for path " ++ show path ++ " is too small (less than 16x16)"
        | width == 16 && height == 16 -> pure fullImg
        | otherwise -> do
            -- take bottom left 16x16
            pixbuf <- fromJustElseMemError <$> Gdk.pixbufNew Gdk.ColorspaceRgb True 8 16 16
            Gdk.pixbufCopyArea fullImg 0 (height - 16) 16 16 pixbuf 0 0
            pure pixbuf
    
    finalPixbuf <- fromJustElseMemError <$> Gdk.pixbufScaleSimple croppedPixBuf 32 32 Gdk.InterpTypeNearest
    Gtk.imageNewFromPixbuf (Just finalPixbuf)

mkTileSelectorPanel :: EditorState -> IO Gtk.Widget
mkTileSelectorPanel EditorState{eventQueue = queue} = do
    flowbox <- new Gtk.FlowBox [#activateOnSingleClick := True, #maxChildrenPerLine := 10]

    dictionary <- runWithEntityWorld queue $ Apecs.get Apecs.global
    let -- remove emptyTile, since it has no graphics and will error if we try to use them
        libTiles          = dictionary^.dictTiles.to (filter (\t -> t^.tileID /= emptyTileID) . Map.elems)
        libTilesBySpriteID = Map.fromList . map (\t -> (getSpriteID t, t)) $ libTiles
        
        getSpriteID t = case t^.tileRenderMode of 
            TRMNormal s -> spriteID s
            TRMRandom s -> spriteID $ Vec.head s
        isTileImage imgID  = Map.member imgID libTilesBySpriteID

        tileImageCounting = dictionary^.dictParsingStore.psCountingIDs.to ((Map.! "SpriteID") . unCounting)

        tileImagePaths    = Map.fromList $ Map.foldlWithKey' appendIfTileImage [] tileImageCounting
        appendIfTileImage acc path idx
            | isTileImage imgID = (imgID, path):acc
            | otherwise         = acc
            where imgID = SpriteID $ fromIntegral idx

    emptyTileIcon <- mkTileIcon "./assets/editor/empty-tile-icon.png"
    tileIcons     <- mapM mkTileIcon tileImagePaths

    let tileIconPairs = (emptyTile, emptyTileIcon) : Map.foldlWithKey' addPair [] libTilesBySpriteID
        -- don't add tiles without any found sprites
        addPair acc imgID tile = case Map.lookup imgID tileIcons of
            Just icon -> (tile, icon):acc
            Nothing   -> acc
        tilesVec      = Vec.fromList . map fst $ tileIconPairs

    forM_ tileIconPairs $ \(_, icon) ->
        Gtk.containerAdd flowbox icon

    Gtk.containerForeach flowbox $ \child -> do
        fbChild <- unsafeCastTo Gtk.FlowBoxChild child
        idx     <- fromIntegral <$> Gtk.flowBoxChildGetIndex fbChild
        set child [#tooltipText := (tilesVec Vec.! idx)^.tileName]
        when (idx == 0) $
            Gtk.flowBoxSelectChild flowbox fbChild

    void $ on flowbox #childActivated $ \child -> do
        idx  <- fromIntegral <$> Gtk.flowBoxChildGetIndex child
        if   idx < 0 || idx >= Vec.length tilesVec
        then putStrLn $ "Tile selector flowbox child widget was activated, but had an index out of range (was "
                     ++ show idx
                     ++  ", must be in (0, "
                     ++ show (Vec.length tilesVec)
                     ++ ")"
        else pushAppEvent queue $ SetSelectedTile (tilesVec Vec.! idx)

    frame <- new Gtk.Frame [#label := "Tiles"]
    Gtk.containerAdd frame flowbox
    Gtk.toWidget frame

mkWorldSpaceToolbar :: EditorState -> IO Gtk.Toolbar
mkWorldSpaceToolbar EditorState{eventQueue = queue, resources = res} = do
    toolbar <- Gtk.toolbarNew

    let btnConfigs :: Vector (Text, ToolMode, Gtk.Image, Gtk.Image)
        btnConfigs = Vec.fromList
            [ ("Play Game",    NoTool, iconToolPlayGame res, iconToolPlayGameActive res)
            , ("Draw Tiles",   TilePlacer emptyTilePlacerState, iconToolTilePlacer res, iconToolTilePlacerActive res)
            , ("Wire Signals", SignalWirer, iconToolSignalWirer res, iconToolSignalWirerActive res)
            ]
        mkButton idx (lbl, mode, icon, activeIcon) = do
            img <- Gtk.imageNewFromPixbuf =<< Gtk.imageGetPixbuf (if idx == 0 then activeIcon else icon)
            btn <- new Gtk.ToolButton [#tooltipText := lbl, #iconWidget := img]
            Gtk.toolbarInsert toolbar btn (-1)
            pure (btn, mode, img, icon, activeIcon)

    widgets <- Vec.imapM mkButton btnConfigs

    forM_ widgets $ \(btn, mode, img, _, activeIcon) ->
        void . on btn #clicked $ do
            -- sets all buttons to use their default images
            -- we set the Pixbuf instead of directly setting the iconWidget, as doing that causes the images to disappear once changed from their default
            forM_ widgets $ \(_, _, wImg, wIcon, _) -> do
                wPixbuf <- Gtk.imageGetPixbuf wIcon
                Gtk.imageSetFromPixbuf wImg wPixbuf
            -- sets this button to use its active image
            pixbuf <- Gtk.imageGetPixbuf activeIcon
            Gtk.imageSetFromPixbuf img pixbuf
            pushAppEvent queue (UseToolMode mode)

    pushAppEvent queue (UseToolMode NoTool)
    pure toolbar

-- | Creates a row that shows the script file, and has a button to edit or delete the script
mkScriptPropertyRow :: Resources -> PropertyRowAdder WorldSpace
mkScriptPropertyRow res rowNum grid wsRef = do
    box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

    fileEntry <- new Gtk.Entry [#editable := False, #canFocus := False]

    let onDelete = do
            Gtk.entrySetText fileEntry "No Script"
            modifyIORef' wsRef $
                worldScript .~ Nothing
        onScriptChange script = do
            Gtk.entrySetText fileEntry $ Lua.sanitizedScriptID script
            modifyIORef' wsRef $
                worldScript ?~ script

    prevScript   <- view worldScript <$> readIORef wsRef
    dialogRunner <- mkEditScriptDialogRunner

    editBtn   <- new Gtk.Button [#image := iconEdit res,   #tooltipText := "Edit Script"]
    deleteBtn <- new Gtk.Button [#image := iconDelete res, #tooltipText := "Remove Script"]

    void $ on editBtn #clicked $ do
        script <- view worldScript <$> readIORef wsRef
        dialogRunner onScriptChange $ fromMaybe Lua.blankScript script
    void $ on deleteBtn #clicked onDelete

    case prevScript of
        Just script -> onScriptChange script
        Nothing     -> onDelete

    Gtk.containerAdd box fileEntry
    Gtk.containerAdd box editBtn
    Gtk.containerAdd box deleteBtn

    let onRefChange _ worldspace = onScriptChange (worldspace^.worldScript.to (fromMaybe Lua.blankScript))
    mkPropertyRowReadOnly "Script" (pure box) onRefChange rowNum grid wsRef

-- | Makes a panel for editing a worldspace and its properties
mkWorldSpacePanel :: EditorState -> IO Gtk.Widget
mkWorldSpacePanel es@EditorState{wsEditState = wsStateRef} = do
    box  <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    grid <- Gtk.gridNew
    wsState <- readIORef wsStateRef
    let wsRef = worldspaceRef wsState

    editProps <- mkPropertyGrid grid wsRef
        [ mkRow "Worldspace ID"       worldID
        , mkRow "Name"                worldName
        , mkScriptPropertyRow         $ resources es
        , mkRow "Player Load Point"   loadPoint
        , mkRow "Save Exact Position" shouldSavePosition
        , mkRow "Initial Palette"     initialPalette
        ]

    writeIORef wsStateRef $ wsState {wsProperties = editProps}
    Gtk.containerAdd box grid

    toolbar <- mkWorldSpaceToolbar es
    Gtk.containerAdd box toolbar

    tileSelector <- mkTileSelectorPanel es
    Gtk.containerAdd box tileSelector

    Gtk.toWidget box
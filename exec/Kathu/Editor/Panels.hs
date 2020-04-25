{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Editor.Panels where

import           Control.Monad              (forM_, void)
import           Data.GI.Base
import           Data.IORef
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vec
import qualified GI.Gtk                     as Gtk

import           Kathu.App.Tools.EventQueue
import           Kathu.App.Tools.ToolMode
import           Kathu.Editor.GtkUtil
import           Kathu.Editor.Resources
import           Kathu.Editor.Types
import           Kathu.Util.Types
import           Kathu.World.WorldSpace     (worldID, worldName)

mkWorldSpaceToolbar :: EditorState -> IO Gtk.Toolbar
mkWorldSpaceToolbar EditorState{eventQueue = queue, resources = res} = do
    toolbar <- Gtk.toolbarNew

    let btnConfigs :: Vector (Text, ToolMode, Gtk.Image, Gtk.Image)
        btnConfigs = Vec.fromList
            [ ("Play Game",  NoTool,     iconToolPlayGame res,   iconToolPlayGameActive res)
            , ("Draw Tiles", TilePlacer, iconToolTilePlacer res, iconToolTilePlacerActive res)
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

mkWorldSpacePanel :: EditorState -> IO Gtk.Widget
mkWorldSpacePanel es@EditorState{wsEditState = wsStateRef} = do
    box  <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    grid <- Gtk.gridNew
    wsState <- readIORef wsStateRef
    let wsRef = worldspaceRef wsState

    editProps <- sequence
        [ addPropertyRowText grid wsRef 0 "Worldspace ID"   (worldID . textIDLens)
        , addPropertyRowText grid wsRef 1 "Worldspace Name" worldName
        ]

    writeIORef wsStateRef $ wsState {wsProperties = editProps}
    Gtk.containerAdd box grid

    toolbar <- mkWorldSpaceToolbar es
    Gtk.containerAdd box toolbar

    Gtk.toWidget box
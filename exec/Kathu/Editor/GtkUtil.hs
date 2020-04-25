{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}

module Kathu.Editor.GtkUtil where

import           Control.Lens               hiding (set)
import           Control.Monad              (void)
import           Data.GI.Base
import           Data.Int
import           Data.IORef
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified GI.Gtk                     as Gtk

import           Kathu.Editor.Types

addPropertyRowText :: Gtk.Grid -> IORef a -> Int32 -> Text -> Lens' a Text -> IO (EditableProperty a)
addPropertyRowText grid ref row name textLens = addPropertyRow grid ref row name #changed Gtk.entryNew onEditWidget onUpdateWidget
    where onEditWidget textWidget a = do
              txt <- Gtk.entryGetText textWidget
              pure (textLens .~ txt $ a)
          onUpdateWidget textWidget a =
              Gtk.entrySetText textWidget (a^.textLens)

addPropertyRow :: Gtk.IsWidget w => Gtk.Grid -> IORef a -> Int32 -> Text -> SignalProxy w Gtk.EditableChangedSignalInfo -> IO w -> (w -> a -> IO a) -> (w -> a -> IO ()) -> IO (EditableProperty a)
addPropertyRow grid ref row name signalProxy editWidget onEditWidgetChange updateWidget = do
    label   <- Gtk.labelNewWithMnemonic (Just name)
    widget  <- editWidget
    void . on widget signalProxy $ do
        value  <- readIORef ref
        value' <- onEditWidgetChange widget value
        writeIORef ref value'

    Gtk.gridAttach grid label 0 row 1 1
    Gtk.gridAttach grid widget 1 row 1 1
    pure $ updateWidget widget

createMenuBar :: MenuBarDescription -> IO Gtk.MenuBar
createMenuBar menuBarDesc = do
    bar <- Gtk.menuBarNew
    mapM_ (makeMenu bar) menuBarDesc
    pure bar
    where makeMenu bar (name, items) = do
              menu <- Gtk.menuNew
              item <- newMenuItemWithMnemonic name
              Gtk.menuItemSetSubmenu item (Just menu)
              Gtk.menuShellAppend bar item
              mapM_ (makeMenuItem menu) items

          makeMenuItem menu (name, onActivate) = do
              item <- newMenuItemWithMnemonic name
              Gtk.menuShellAppend menu item
              Gtk.onMenuItemActivate item onActivate

          newMenuItemWithMnemonic l | T.any (=='_') l = Gtk.menuItemNewWithMnemonic l
                                    | otherwise       = Gtk.menuItemNewWithLabel l

showFileChooser :: Gtk.ApplicationWindow -> Gtk.FileChooserAction -> Text -> Text -> Text -> IO (Maybe String)
showFileChooser window action title filterName filterExt = do
    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterSetName fileFilter (Just filterName)
    Gtk.fileFilterAddPattern fileFilter filterExt

    dialog <- Gtk.fileChooserNativeNew (Just title) (Just window) action Nothing Nothing
    set dialog [#filter := fileFilter]

    response <- Gtk.nativeDialogRun dialog
    Gtk.nativeDialogHide dialog

    case toEnum (fromIntegral response) of
        Gtk.ResponseTypeAccept -> Gtk.fileChooserGetFilename dialog
        _                      -> pure Nothing
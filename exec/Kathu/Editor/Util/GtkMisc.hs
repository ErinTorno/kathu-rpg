{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Util.GtkMisc where

import           Control.Monad              (forM_)
import           Data.GI.Base
import           Data.GI.Base.GType         (gtypeStrv)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified GI.Gtk                     as Gtk

import           Kathu.Editor.Types

mkMenuBar :: MenuBarDescription -> IO Gtk.MenuBar
mkMenuBar menuBarDesc = do
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

mkEntryCompletion :: Foldable t => t Text -> IO Gtk.EntryCompletion
mkEntryCompletion rows = do
    list <- Gtk.listStoreNew [gtypeStrv]
    forM_ rows $ \row -> do
        txtVal  <- toGValue (Just row)
        newIter <- Gtk.listStoreAppend list
        Gtk.listStoreSetValue list newIter 0 txtVal
    new Gtk.EntryCompletion [#model := list]

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
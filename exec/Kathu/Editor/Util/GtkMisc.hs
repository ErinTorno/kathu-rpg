{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Util.GtkMisc where

import           Control.Monad              (forM_, void, when)
import           Data.GI.Base
import           Data.GI.Base.GType         (gtypeString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified GI.GObject.Functions       as GObj
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
    list <- Gtk.listStoreNew [gtypeString]
    forM_ rows $ \row -> do
        txtVal  <- toGValue (Just row)
        newIter <- Gtk.listStoreAppend list
        Gtk.listStoreSetValue list newIter 0 txtVal
    entryCompletion <- new Gtk.EntryCompletion [#model := list]
    Gtk.entryCompletionSetTextColumn entryCompletion 0
    pure entryCompletion

showErrorDialog :: Text -> IO ()
showErrorDialog msg = do
    dialog <- new Gtk.MessageDialog [#messageType := Gtk.MessageTypeError, #text := msg]
    void $ Gtk.dialogAddButton dialog "Accept" 1
    void $ Gtk.dialogRun dialog
    Gtk.widgetDestroy dialog

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

-- | Contructs an Entry that refuses to allow text that cannot be parsed using the given parser
mkParsableEntry :: (Text -> Maybe a) -> IO Gtk.Entry
mkParsableEntry tryParse = do
    entry       <- new Gtk.Entry []
    -- kills a signal and prevents changes the the entry's text when it fails to parse
    let cancelInvalid sigName newTxt = case tryParse newTxt of
            Just _  -> pure ()
            Nothing -> GObj.signalStopEmissionByName entry sigName

    void $ on entry #insertText $ \newTextPart _ pos -> do
        prevText <- get entry #text
        let (beginTxt, endTxt) = T.splitAt (fromIntegral pos) prevText
            newTxt             = T.concat [beginTxt, newTextPart, endTxt]
        cancelInvalid "insert-text" newTxt
        pure pos

    void $ on entry #deleteText $ \startPos endPos ->
        when (startPos >= 0 && endPos >= 0) $ do
            prevText <- get entry #text
            let beginTxt = T.take (fromIntegral startPos) prevText
                endTxt   = T.drop (fromIntegral endPos)   prevText
                newTxt   = beginTxt `T.append` endTxt
            cancelInvalid "delete-text" newTxt
    pure entry
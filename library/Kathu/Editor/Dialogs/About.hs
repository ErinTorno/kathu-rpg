{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Dialogs.About where

import           Control.Monad              (void)
import           Data.GI.Base
import qualified Data.Text    as T
import           Data.Version (showVersion)
import qualified GI.Gtk       as Gtk
import           Paths_kathu  (version)

-- | Shows the about dialog for this program
showAboutDialog :: IO ()
showAboutDialog = do
    dialog <- new Gtk.AboutDialog
        [ #authors     := ["Erin Torno"]
        , #comments    := "An editor for creating and working with Kathu's .world files"
        , #programName := "Kathu Editor"
        , #version     := T.pack (showVersion version)
        ]
    void $ Gtk.dialogRun dialog
    Gtk.widgetDestroy dialog
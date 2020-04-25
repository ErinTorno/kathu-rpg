{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.Editor.Dialogs where

import           Paths_kathu                (version)
import           Data.Version               (showVersion)

import           Control.Monad              (void)
import           Data.GI.Base
import qualified Data.Text                  as T
import qualified GI.Gtk                     as Gtk

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
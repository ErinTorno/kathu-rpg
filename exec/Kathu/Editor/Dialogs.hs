{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Dialogs where

import           Paths_kathu                (version)
import           Data.Version               (showVersion)

import           Control.Lens               hiding (set)
import           Control.Monad              (forM, forM_, void, when)
import           Data.GI.Base
import           Data.IORef
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified GI.Gtk                     as Gtk

import           Kathu.Editor.GtkUtil
import           Kathu.Editor.Types
import qualified Kathu.Scripting.Lua        as Lua
import           Kathu.Scripting.Event

-- | Creates a dialog for editing a script, and returns an IO action that displays, saves, and then hides the dialog
createEditScriptDialogRunner :: (Lua.Script -> IO ()) -> IO (DialogRunner Lua.Script)
createEditScriptDialogRunner onScriptChange = do
    dialog    <- new Gtk.Dialog [#title := "Edit Script", #modal := True]
    scriptRef <- newIORef Lua.blankScript

    grid <- new Gtk.Grid []
    editProps :: [EditableProperty Lua.Script] <- sequence
        [ addPropertyRowBool grid scriptRef 0 "Is a singleton?" Lua.isSingleton      
        ]

    eventList  <- new Gtk.ListBox []
    buttons <- forM (Map.assocs allEvents) $ \(name, event) -> do
        btn <- new Gtk.CheckButton [#label := T.replace "-" " " name]
        Gtk.containerAdd eventList btn
        -- toggles the event flag
        void $ on btn #toggled $
            modifyIORef' scriptRef $ over Lua.scriptEventFlags $ \flags ->
                setEventFlagEnabled event flags $ not (isEventSet event flags)
        pure (btn, event)

    dialogContent <- Gtk.dialogGetContentArea dialog
    Gtk.containerAdd dialogContent grid
    Gtk.containerAdd dialogContent eventList
    void $ Gtk.dialogAddButton dialog "Save" 1
    void $ Gtk.dialogAddButton dialog "Close" 2

    -- this dialog crashes when destroyed, due to the callback handlers on the buttons inside of it
    -- removing the callback handlers also crash, so instead we use this style to show and hide it instead, never destroying it
    let runUntilClose prevScript = do
            writeIORef scriptRef prevScript

            mapM_ ($prevScript) editProps

            forM_ buttons $ \(btn, event) ->
                set btn [#active := isEventSet event $ prevScript^.Lua.scriptEventFlags]

            Gtk.widgetShowAll dialogContent
            responseID <- Gtk.dialogRun dialog
            case responseID of
                1 -> do
                    finalScript <- readIORef scriptRef
                    when (finalScript /= prevScript) $
                        onScriptChange finalScript
                    runUntilClose finalScript
                _ -> do
                    Gtk.widgetHide dialogContent
                    Gtk.widgetHide dialog
    pure runUntilClose

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
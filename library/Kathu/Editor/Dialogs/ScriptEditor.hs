{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Dialogs.ScriptEditor where

import           Control.Lens               hiding (set)
import           Control.Monad              (forM, forM_, void, when)
import           Data.GI.Base
import           Data.IORef
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified GI.Gtk                     as Gtk

import           Kathu.Editor.Types
import           Kathu.Editor.Util.PropertyGrid
import qualified Kathu.Scripting.Lua        as Lua
import           Kathu.Scripting.Event

-- | Creates a dialog for editing a script, and returns an IO action that displays, saves, and then hides the dialog
mkEditScriptDialogRunner :: IO (DialogRunner Lua.Script)
mkEditScriptDialogRunner = do
    dialog    <- new Gtk.Dialog [#title := "Edit Script", #modal := True]
    scriptRef <- newIORef Lua.blankScript

    grid <- new Gtk.Grid []
    editProps <- mkPropertyGrid grid scriptRef
        [ mkRow "Is a singleton?" Lua.isSingleton      
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
    eventFrame    <- new Gtk.Frame [#label := "Watched Events"]
    Gtk.containerAdd eventFrame eventList

    Gtk.containerAdd dialogContent grid
    Gtk.containerAdd dialogContent eventFrame
    void $ Gtk.dialogAddButton dialog "Save" 1
    void $ Gtk.dialogAddButton dialog "Close" 2

    -- this dialog crashes when destroyed, due to the callback handlers on the buttons inside of it
    -- removing the callback handlers also crash, so instead we use this style to show and hide it instead, never destroying it
    let runUntilClose onScriptChange prevScript = do
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
                    runUntilClose onScriptChange finalScript
                _ -> do
                    Gtk.widgetHide dialogContent
                    Gtk.widgetHide dialog
    pure runUntilClose
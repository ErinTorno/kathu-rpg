{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Kathu.Editor.Main (shouldRunEditor, start) where

import           Control.Concurrent         (forkIO)
import           Control.Monad              (forM_, void)
import           Data.GI.Base
import           Data.IORef
import           Data.List                  (isSuffixOf)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk
import qualified SDL
import           System.FilePath            (takeFileName)

import qualified Kathu.App.Main             as Kathu
import           Kathu.App.Data.Settings
import           Kathu.App.Graphics.Image
import           Kathu.App.Tools.Commands
import           Kathu.App.Tools.EventQueue
import           Kathu.Editor.Dialogs
import           Kathu.Editor.File
import           Kathu.Editor.GtkUtil
import           Kathu.Editor.Panels
import           Kathu.Editor.Resources
import           Kathu.Editor.Types
import           Kathu.World.WorldSpace

mkMenuBarDescription :: EditorState -> MenuBarDescription
mkMenuBarDescription es@EditorState{editorApp = app, editorWindow = window, eventQueue = queue} =
    [ ("_File", [ ("New Worldspace", pushAppEvent queue NewWorldSpace)
                , ("_Open",   onFileLoad es =<< showFileChooser window Gtk.FileChooserActionOpen "Open WorldSpace File" "WorldSpace files" "*.world")
                , ("_Save",   putStrLn "Save pressed")
                , ("Save As", putStrLn "Save As pressed")
                , ("_Quit",   void $ onApplicationClose app)])
    , ("_Help", [ ("_About",  showAboutDialog)])]

-- Later can include save unsaved work option
onApplicationClose :: Gtk.Application -> IO Bool
onApplicationClose app = Gio.applicationQuit app >> pure True

onFileLoad :: EditorState -> Maybe FilePath -> IO ()
onFileLoad es@EditorState{eventQueue = queue} maybeFile =
    forM_ maybeFile $ \f ->
        loadWorldSpace queue f
        >>= setActiveWorldSpace es (T.pack $ takeFileName f)


setActiveWorldSpace :: EditorState -> Text -> WorldSpace ImageID -> IO ()
setActiveWorldSpace EditorState{editorWindow = window, eventQueue = queue, wsEditState = wsStateRef} name worldspace = do
    let windowName = T.concat [Kathu.appName, " ~ ", name]
    set window [#title := windowName]

    pushAppEvent queue (LoadWorldSpace worldspace)
    -- we update all editable property watchers to use the new values
    wsProps <- wsProperties <$> readIORef wsStateRef
    mapM_ ($worldspace) wsProps

handleEditorEvents :: EditorState -> IO Bool
handleEditorEvents editorSt = do
    events <- pollEditorEvents (eventQueue editorSt)
    mapM_ (handleEditorEvent editorSt) events
    pure True

handleEditorEvent :: EditorState -> EditorEvent -> IO ()
handleEditorEvent EditorState{editorApp = app} event = case event of
    TryToCloseEditor -> void $ onApplicationClose app

activateApp :: Gtk.Application -> EventQueue -> [String] -> Gio.ApplicationActivateCallback
activateApp app queue args = do
    maybeSettings <- Gtk.settingsGetDefault
    forM_ maybeSettings $ \settings ->
        set settings [ #gtkApplicationPreferDarkTheme := True ]

    window <- new Gtk.ApplicationWindow
        [ #application   := app
        , #title         := Kathu.appName
        , #defaultHeight := 200
        , #defaultWidth  := 380
        ]
    Gtk.windowSetIconFromFile window appIconPath

    res      <- loadResources 
    wsRef    <- newIORef emptyWorldSpace
    wsEditSt <- newIORef $ WSEditState wsRef []
    let editorState = EditorState app window queue wsEditSt res

    -- For some reason letting it close on its own can generate a lot of access violations that don't occur when applicationQuit is called
    on window #deleteEvent $ \_ ->
        onApplicationClose app

    mainVBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    menuBar <- createMenuBar (mkMenuBarDescription editorState)
    Gtk.containerAdd mainVBox menuBar

    worldspacePanel <- mkWorldSpacePanel editorState
    Gtk.containerAdd mainVBox worldspacePanel

    Gtk.setContainerChild window mainVBox

    setActiveWorldSpace editorState "New File" emptyWorldSpace
    onFileLoad editorState $ getWorldFile args

    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (handleEditorEvents editorState)

    #showAll window

shouldRunEditor :: [String] -> Bool
shouldRunEditor = any isEditorArg
    where isEditorArg arg | arg == "--editor"         = True
                          | ".world" `isSuffixOf` arg = True
                          | otherwise                 = False

getWorldFile :: [String] -> Maybe String
getWorldFile [] = Nothing
getWorldFile (x:xs) | ".world" `isSuffixOf` x = Just x
                    | otherwise               = getWorldFile xs

start :: [String] -> IO ()
start args = do
    queue <- newEventQueue
    app <- new Gtk.Application [ #applicationId := "haskell-gi.kathu"
                               , #flags := [Gio.ApplicationFlagsFlagsNone]
                               ]
    on app #activate $ activateApp app queue args

    let updateSettings s = s {initialWorld = Nothing}

    forkIO . Kathu.startWith updateSettings $ \(Kathu.RenderInfo _ renderer buffer settings) world -> do
        curTime <- SDL.ticks
        -- need to put this first or else it will get stuct waiting for the MVar to be filled
        putEntityWorld world queue
        commandState <- newCommandState
        Kathu.runForEventQueue queue commandState (Kathu.renderDelay settings) renderer buffer curTime curTime

    Gio.applicationRun app Nothing

    pure ()
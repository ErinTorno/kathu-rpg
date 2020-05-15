{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE OverloadedLists #-}

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
mkMenuBarDescription es@EditorState{editorWindow = window, eventQueue = queue} =
    [ ("_File", [ ("New Worldspace", onNewFile es)
                , ("_Open",          onFileLoad es =<< showFileChooser window Gtk.FileChooserActionOpen "Open WorldSpace File" "WorldSpace files" "*.world")
                , ("_Save",          saveWorldSpace es SaveIfHasFile)
                , ("Save As",        saveWorldSpace es ForceSaveAs)
                , ("_Quit",          onApplicationClose es)])
    , ("_Game", [ ("Toggle _Debug",  pushAppEvent queue ToggleDebug)])
    , ("_Help", [ ("_About",         showAboutDialog)])]

-- Later can include save unsaved work option
onApplicationClose :: EditorState -> IO ()
onApplicationClose EditorState{editorApp = app, eventQueue = queue} = do
    Gio.applicationQuit app
    -- We let the game close itself, as it is running on the main threat we forked from
    pushAppEvent queue TryToQuitGame

onNewFile :: EditorState -> IO ()
onNewFile es@EditorState{wsEditState = wsEditStRef} = do
    modifyIORef' wsEditStRef $ \wsEditSt ->
        wsEditSt {wsFilePath = Nothing}
    setActiveWorldSpace es "new worldspace" emptyWorldSpace

onFileLoad :: EditorState -> Maybe FilePath -> IO ()
onFileLoad es@EditorState{eventQueue = queue, wsEditState = wsEditStRef} maybeFile =
    forM_ maybeFile $ \f -> do
        ws <- loadWorldSpace queue f

        modifyIORef' wsEditStRef $ \wsEditSt ->
            wsEditSt {wsFilePath = Just f}

        setActiveWorldSpace es (T.pack $ takeFileName f) ws

setActiveWorldSpace :: EditorState -> Text -> WorldSpace ImageID -> IO ()
setActiveWorldSpace EditorState{editorWindow = window, eventQueue = queue, wsEditState = wsStateRef} name worldspace = do
    let windowName = T.concat [Kathu.appName, " ~ ", name]
    set window [#title := windowName]

    pushAppEvent queue (LoadWorldSpace worldspace)

    wsState <- readIORef wsStateRef
    let wsProps = wsProperties  wsState
        wsRef   = worldspaceRef wsState
    writeIORef wsRef worldspace
    
    -- we update all editable property watchers to use the new values
    mapM_ ($worldspace) wsProps

handleEditorEvents :: EditorState -> IO Bool
handleEditorEvents editorSt = do
    events <- pollEditorEvents (eventQueue editorSt)
    mapM_ (handleEditorEvent editorSt) events
    pure True

handleEditorEvent :: EditorState -> EditorEvent -> IO ()
handleEditorEvent _ event = case event of
    DummyEditorEvent -> putStrLn "DummyEditorEvent"

activateApp :: Gtk.Application -> EventQueue -> [String] -> Gio.ApplicationActivateCallback
activateApp app queue args = do
    maybeSettings <- Gtk.settingsGetDefault
    forM_ maybeSettings $ \settings ->
        set settings [ #gtkApplicationPreferDarkTheme := True ]

    window <- new Gtk.ApplicationWindow
        [ #application   := app
        , #title         := Kathu.appName
        , #defaultHeight := 300
        , #defaultWidth  := 340
        ]
    Gtk.windowSetIconFromFile window appIconPath

    res       <- loadResources 
    wsRef     <- newIORef emptyWorldSpace
    wsEditSt  <- newIORef $ WSEditState wsRef [] Nothing
    let editorState = EditorState app window queue wsEditSt res

    on window #deleteEvent $ \_ ->
        onApplicationClose editorState >> pure True

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

    -- don't load any world yet, and force debug to be usable
    let updateSettings s = s {initialWorld = Nothing, canUseDebug = True}

    curTime <- SDL.ticks
    commandState <- newCommandState

    forkIO $ do
        app <- new Gtk.Application [ #applicationId := "haskell-gi.kathu"
                                   , #flags := [Gio.ApplicationFlagsFlagsNone]
                                   ]
        on app #activate $ activateApp app queue args

        void $ Gio.applicationRun app Nothing

    Kathu.startWith updateSettings $ \(Kathu.RenderInfo _ renderer buffer settings) world -> do
        -- need to put this first or else it will get stuct waiting for the MVar to be filled
        putEntityWorld world queue
        Kathu.runForEventQueue queue commandState (Kathu.renderDelay settings) renderer buffer curTime curTime

    pure ()
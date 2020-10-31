{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Main (shouldRunEditor, start) where

import           Apecs                      (($=), ask, global, lift)
import qualified Apecs
import           Control.Concurrent         (forkIO)
import           Control.Lens               hiding (set)
import           Control.Monad              (forM_, void, when)
import           Data.GI.Base
import           Data.IORef
import           Data.List                  (isSuffixOf)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk
import           System.FilePath            (takeFileName)
import           Verda.App
import           Verda.World

import           Kathu.App.System           (EntityWorld)
import           Kathu.App.Tools.Commands
import           Kathu.App.Tools.EventHandler
import           Kathu.App.Tools.EventQueue
import           Kathu.App.Tools.ToolMode
import           Kathu.App.Tools.ToolSystem (addToolSystemExtension, runToolMode)
import           Kathu.Config.Settings
import           Kathu.Editor.Dialogs
import           Kathu.Editor.Dialogs.Entity
import           Kathu.Editor.File
import           Kathu.Editor.Panels
import           Kathu.Editor.Resources
import           Kathu.Editor.Types
import           Kathu.Editor.Util.GtkMisc
import           Kathu.Entity.System        (IncludeEditorInfo(..))
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
    -- We let the game close itself, as it is running on the main thread we forked from
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

setActiveWorldSpace :: EditorState -> Text -> WorldSpace -> IO ()
setActiveWorldSpace EditorState{editorWindow = window, eventQueue = queue, wsEditState = wsStateRef} name worldspace = do
    let windowName = T.concat ["Kathu ~ ", name]
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
handleEditorEvent EditorState{eventQueue = queue, heldDialogRunners = dialogs} event = case event of
    EditEntityInstance ety instanced ->
        let runDialog = runInstancedPrototype dialogs
            config    = mkInstancedEntityConfig ety instanced
            onSaveEty newEty = do
                forM_ (newEty^.iecOriginalEntity) $ \origEty ->
                    pushAppEvent queue (DestroyEntity origEty)
                pushAppEvent queue (PlaceEntity (newEty^.iecInstancedPrefab))
         in runDialog onSaveEty config

newDialogRunners :: EventQueue -> IO HeldDialogRunners
newDialogRunners queue = HeldDialogRunners
                     <$> newInstancedPrototypeDialogRunner queue

activateApp :: Gtk.Application -> EventQueue -> [String] -> Gio.ApplicationActivateCallback
activateApp app queue args = do
    maybeSettings <- Gtk.settingsGetDefault
    forM_ maybeSettings $ \settings ->
        set settings [ #gtkApplicationPreferDarkTheme := True ]

    window <- new Gtk.ApplicationWindow
        [ #application   := app
        , #title         := "Kathu Editor"
        , #defaultHeight := 420
        , #defaultWidth  := 340
        ]
    Gtk.windowSetIconFromFile window appIconPath

    res       <- loadResources 
    wsRef     <- newIORef emptyWorldSpace
    wsEditSt  <- newIORef $ WSEditState wsRef [] Nothing
    dialogRunners <- newDialogRunners queue
    let editorState = EditorState app window queue wsEditSt res dialogRunners

    on window #deleteEvent $ \_ ->
        onApplicationClose editorState >> pure True

    mainVBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    menuBar <- mkMenuBar (mkMenuBarDescription editorState)
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

start :: [String] -> AppConfig EntityWorld -> IO ()
start args baseAppConfig = do
    queue <- newEventQueue
    commandState <- newCommandState
    -- don't load any world yet, and force debug to be usable
    let updateSettings s = s {initialWorld = Nothing, canUseDebug = True}

    forkIO $ do
        app <- new Gtk.Application [ #applicationId := "haskell-gi.kathu"
                                   , #flags := [Gio.ApplicationFlagsFlagsNone]
                                   ]
        on app #activate $ activateApp app queue args

        void $ Gio.applicationRun app Nothing

    Verda.App.run $ baseAppConfig
        { concurrentWorldVar = do
            world <- ask
            lift $ putEntityWorld world queue
            pure $ Just (entityWorld queue)
        , initWorld = \window renderer -> do
            initWorld baseAppConfig window renderer
            Apecs.set Apecs.global (IncludeEditorInfo True)
            addToolSystemExtension
        , runGame = \dT -> do
            handleEvents queue commandState
            runToolMode queue commandState
            runState  <- Apecs.get global
            shouldRun <- not . usesFreeCam <$> Apecs.get global
            when (runState /= Quitting) $
                global $= if shouldRun then Running else Paused
            runGame baseAppConfig dT
        }
    pure ()
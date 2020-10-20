{-# LANGUAGE OverloadedLists #-}

module Kathu.Editor.Dialogs.Entity where

import qualified Apecs
import           Control.Lens                   hiding (set)
import           Control.Monad                  (void)
import           Data.GI.Base
import           Data.IORef
import qualified Data.Map                       as Map
import qualified Data.Text                  as T
import qualified GI.Gtk                         as Gtk

import           Kathu.App.Data.Library
import           Kathu.App.Tools.EventQueue
import           Kathu.Editor.Util.GtkMisc
import           Kathu.Editor.Types
import           Kathu.Editor.Util.PropertyGrid
import           Kathu.World.WorldSpace
import           Verda.Util.Types

newInstancedPrototypeDialogRunner :: EventQueue -> IO (DialogRunner InstancedEntityConfig)
newInstancedPrototypeDialogRunner queue = do
    instanceRef <- newIORef emptyInstancedEntityConfig
    library     <- runWithEntityWorld queue $ Apecs.get Apecs.global

    let allPrototypes = library^.prototypes
        addComplete w = do
            entityCompletion <- mkEntryCompletion $ map unID (Map.keys allPrototypes)
            Gtk.entrySetCompletion w (Just entityCompletion)

    grid <- new Gtk.Grid []
    editProps <- mkPropertyGrid grid instanceRef
        [ mkRow "Instance ID"     $ iecInstancedPrototype . instanceID
        , mkRowWith MutableRow addComplete "Base Entity" (iecPrototypeID . textIDLens)
        , mkRow "Position"        $ iecInstancedPrototype . spawnLocation
        , mkRow "Emits Signal"    $ iecInstancedPrototype . wireSignalEmitter . non ""
        , mkRow "Receives Signal" $ iecInstancedPrototype . wireSignalReceiver . non ""
        -- instanceConfig     :: !(IDMap WorldVariable)
        ]
    
    dialog        <- new Gtk.Dialog [#title := "Edit Entity"]
    dialogContent <- Gtk.dialogGetContentArea dialog
    Gtk.containerAdd dialogContent grid
    void $ Gtk.dialogAddButton dialog "Accept Changes" 1
    void $ Gtk.dialogAddButton dialog "Cancel" 2

    let runDialog onInstanceChange origInstance = do
            writeIORef instanceRef origInstance

            mapM_ ($origInstance) editProps

            Gtk.widgetShowAll dialogContent
            responseID <- Gtk.dialogRun dialog
            case responseID of
                1 -> do
                    finalInstance <- readIORef instanceRef
                    let newBaseEtyID = finalInstance^.iecPrototypeID
                    case Map.lookup newBaseEtyID allPrototypes of
                        Nothing    -> do
                            showErrorDialog . T.pack $ "Unknown base entity " ++ show newBaseEtyID
                            runDialog onInstanceChange finalInstance
                        Just proto ->
                            onInstanceChange $ (iecInstancedPrototype . basePrototype .~ proto) finalInstance
                _ -> pure ()
            pushAppEvent queue FinishEditingEntityInstance
            Gtk.widgetHide dialogContent
            Gtk.widgetHide dialog
    pure runDialog
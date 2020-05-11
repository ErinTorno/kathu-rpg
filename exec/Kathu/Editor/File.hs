module Kathu.Editor.File where

import           Apecs                      hiding (set)
import           Control.Lens
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                  as T
import qualified GI.Gtk                     as Gtk

import           Kathu.App.Data.Library
import           Kathu.App.Graphics.Image   (ImageID)
import           Kathu.App.Tools.EventQueue
import           Kathu.Editor.Types
import           Kathu.Editor.GtkUtil       (showFileChooser)
import           Kathu.IO.Directory
import           Kathu.IO.File
import           Kathu.Util.Dependency
import           Kathu.World.WorldSpace

toAssetRelativePath :: FilePath -> FilePath -> Either String FilePath
toAssetRelativePath currentWorkingDirectory filePath = 
    let curWorkDir = T.concat [T.pack currentWorkingDirectory, "/", assetPath]
        file       = T.pack filePath
        stripped   = T.stripPrefix curWorkDir file
     in case stripped of
         Just f  -> Right $ T.unpack f
         Nothing -> Left "toAssetRelativePath FilePath is not a part of the asset directory"

saveWorldSpace :: EditorState -> SaveType -> IO ()
saveWorldSpace EditorState{ editorWindow = window
                          , eventQueue   = queue
                          , wsEditState  = wsEditStRef} saveType = do
    wsSt@WSEditState{worldspaceRef = wsRef, wsFilePath = wsFile} <- readIORef wsEditStRef
    
    maybeFP <- if   saveType == ForceSaveAs || isNothing wsFile
               then showFileChooser window Gtk.FileChooserActionSave "Open WorldSpace File" "WorldSpace files" "*.world"
               else pure wsFile
    case maybeFP of
        Nothing -> pure ()
        Just filePath -> do
            writeIORef wsEditStRef $ wsSt {wsFilePath = Just filePath}

            worldspace <- readIORef wsRef

            allTiles <- runWithEntityWorld queue $ get global
            wsValue <- encodeValueForWorldSpace allTiles worldspace

            case fileFormatPrefix filePath of
                FormatYAML -> saveYamlToFileWithFieldOrder worldspaceFieldOrder filePath wsValue
                format     -> saveToFile format filePath wsValue

loadWorldSpace :: EventQueue -> FilePath -> IO (WorldSpace ImageID)
loadWorldSpace queue file = runWithEntityWorld queue $ do
    library  <- get global
    worldDep <- lift $ loadFromFileDP file
    (worldspace, store') <- runDependency worldDep (library^.kathuStore)
    global   $= set kathuStore store' library
    pure worldspace
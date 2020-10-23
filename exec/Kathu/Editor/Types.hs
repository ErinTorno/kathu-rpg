{-# LANGUAGE TemplateHaskell #-}

module Kathu.Editor.Types where

import           Apecs                      (Entity)
import           Control.Lens
import           Data.IORef
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified GI.Gtk                     as Gtk

import           Kathu.App.Tools.EventQueue
import           Kathu.Editor.Resources     (Resources)
import           Kathu.Entity.Prefab        (prefabID)
import           Kathu.World.WorldSpace
import           Verda.Util.Types

type MenuBarDescription = [(Text, [(Text, Gtk.MenuItemActivateCallback)])]

type EditableProperty a = a -> IO ()

data SaveType = ForceSaveAs | SaveIfHasFile deriving Eq

type DialogRunner a = (a -> IO ()) -- result consumer
                   -> a            -- initial value
                   -> IO ()

data InstancedEntityConfig = InstancedEntityConfig
    { _iecOriginalEntity     :: !(Maybe Entity)
    , _iecPrefabID           :: !Identifier
    , _iecInstancedPrefab    :: !InstancedPrefab
    }
makeLenses ''InstancedEntityConfig

emptyInstancedEntityConfig :: InstancedEntityConfig
emptyInstancedEntityConfig = InstancedEntityConfig Nothing "" emptyInstancedPrefab

mkInstancedEntityConfig :: Entity -> InstancedPrefab -> InstancedEntityConfig
mkInstancedEntityConfig ety prefab = InstancedEntityConfig (Just ety) pID prefab
    where pID = prefab^.basePrefab.to prefabID

data WSEditState = WSEditState
    { worldspaceRef :: !(IORef WorldSpace)
    , wsProperties  :: !(Vector (EditableProperty WorldSpace))
    , wsFilePath    :: !(Maybe FilePath)
    }

newtype HeldDialogRunners = HeldDialogRunners
    { runInstancedPrototype :: DialogRunner InstancedEntityConfig
    }

data EditorState = EditorState
    { editorApp         :: !Gtk.Application
    , editorWindow      :: !Gtk.ApplicationWindow
    , eventQueue        :: !EventQueue
    , wsEditState       :: !(IORef WSEditState)
    , resources         :: !Resources
    , heldDialogRunners :: HeldDialogRunners
    }
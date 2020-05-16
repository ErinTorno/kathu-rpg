module Kathu.Editor.Types where

import           Data.IORef
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified GI.Gtk                     as Gtk

import           Kathu.App.Graphics.Image
import           Kathu.App.Tools.EventQueue
import           Kathu.Editor.Resources     (Resources)
import           Kathu.World.WorldSpace

type MenuBarDescription = [(Text, [(Text, Gtk.MenuItemActivateCallback)])]

type EditableProperty a = a -> IO ()

data SaveType = ForceSaveAs | SaveIfHasFile deriving Eq

type DialogRunner a = a -> IO ()

data WSEditState = WSEditState
    { worldspaceRef :: !(IORef (WorldSpace ImageID))
    , wsProperties  :: !(Vector (EditableProperty (WorldSpace ImageID)))
    , wsFilePath    :: !(Maybe FilePath)
    }

data EditorState = EditorState
    { editorApp     :: !Gtk.Application
    , editorWindow  :: !Gtk.ApplicationWindow
    , eventQueue    :: !EventQueue
    , wsEditState   :: !(IORef WSEditState)
    , resources     :: !Resources
    }
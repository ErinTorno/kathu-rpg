{-# LANGUAGE TemplateHaskell #-}

module Kathu.Editor.Types where

import           Apecs                      (Entity)
import           Control.Lens
import           Data.IORef
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified GI.Gtk                     as Gtk
import           Verda.Graphics.Sprites     (SpriteID)

import           Kathu.App.Tools.EventQueue
import           Kathu.Editor.Resources     (Resources)
import           Kathu.Entity.Prototype     (getPrototypeID)
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
    , _iecPrototypeID        :: !Identifier
    , _iecInstancedPrototype :: !(InstancedPrototype SpriteID)
    }
makeLenses ''InstancedEntityConfig

emptyInstancedEntityConfig :: InstancedEntityConfig
emptyInstancedEntityConfig = InstancedEntityConfig Nothing "" emptyInstancedPrototype

mkInstancedEntityConfig :: Entity -> InstancedPrototype SpriteID -> InstancedEntityConfig
mkInstancedEntityConfig ety proto = InstancedEntityConfig (Just ety) protoID proto
    where protoID = proto^.basePrototype.to getPrototypeID

data WSEditState = WSEditState
    { worldspaceRef :: !(IORef (WorldSpace SpriteID))
    , wsProperties  :: !(Vector (EditableProperty (WorldSpace SpriteID)))
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
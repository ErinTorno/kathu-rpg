module Kathu.Editor.Resources where

import qualified GI.Gtk                     as Gtk

import           Kathu.IO.Directory         (assetPath)

editorAssetPath :: String
editorAssetPath = assetPath ++ "/editor/"

appIconPath :: String
appIconPath = editorAssetPath ++ "icon-large.png"

data Resources = Resources
    { iconToolPlayGame         :: Gtk.Image
    , iconToolPlayGameActive   :: Gtk.Image
    , iconToolTilePlacer       :: Gtk.Image
    , iconToolTilePlacerActive :: Gtk.Image
    }

loadResources :: IO Resources
loadResources = Resources
    <$> Gtk.imageNewFromFile (editorAssetPath ++ "tool-play-game.png")
    <*> Gtk.imageNewFromFile (editorAssetPath ++ "tool-play-game-active.png")
    <*> Gtk.imageNewFromFile (editorAssetPath ++ "tool-tile-placer.png")
    <*> Gtk.imageNewFromFile (editorAssetPath ++ "tool-tile-placer-active.png")
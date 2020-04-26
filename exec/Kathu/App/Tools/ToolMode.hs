{-# LANGUAGE TypeFamilies #-}

module Kathu.App.Tools.ToolMode where

import Apecs
import Linear.V2                (V2(..))

import Kathu.App.Graphics.Image (ImageID)
import Kathu.World.Tile

data TilePlacerState = TilePlacerState
    { selectedTile :: !(Tile ImageID)
    , lastPlacePos :: !(Maybe (V2 Int))
    }

mkTilePlacerState :: Tile ImageID -> TilePlacerState
mkTilePlacerState t = TilePlacerState t Nothing

-- | A global component that determines whether normal camera rules are overriden and a mouse-based movie should be used
data ToolMode
    = NoTool
    | TilePlacer TilePlacerState

instance Semigroup ToolMode where (<>) = mappend
instance Monoid ToolMode where mempty = NoTool
instance Component ToolMode where type Storage ToolMode = Global ToolMode

usesFreeCam :: ToolMode -> Bool
usesFreeCam NoTool = False
usesFreeCam _      = True
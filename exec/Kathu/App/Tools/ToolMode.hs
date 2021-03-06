module Kathu.App.Tools.ToolMode where

import Apecs
import Data.Word
import Linear.V2                (V2(..))

import Kathu.App.Graphics.Image (ImageID)
import Kathu.World.Tile

newtype TilePlacerState = TilePlacerState {tileSelectorEty :: Maybe Entity}

emptyTilePlacerState :: TilePlacerState
emptyTilePlacerState = TilePlacerState Nothing

-- | A global component that determines whether normal camera rules are overriden and a mouse-based movie should be used
data ToolMode
    = NoTool
    | TilePlacer TilePlacerState
    | SignalWirer

instance Semigroup ToolMode where (<>) = mappend
instance Monoid ToolMode where mempty = NoTool
instance Component ToolMode where type Storage ToolMode = Global ToolMode

isNoTool :: ToolMode -> Bool
isNoTool NoTool = True
isNoTool _      = False

usesFreeCam :: ToolMode -> Bool
usesFreeCam NoTool = False
usesFreeCam _      = True

shouldShowGrid :: ToolMode -> Bool
shouldShowGrid NoTool = False
shouldShowGrid _      = True

canEditEntities :: ToolMode -> Bool
canEditEntities NoTool = False
canEditEntities _      = True

-- | Returns True if both modes are the same type, regardless of configuration
isSameMode :: ToolMode -> ToolMode -> Bool
isSameMode NoTool       NoTool       = True
isSameMode TilePlacer{} TilePlacer{} = True
isSameMode SignalWirer  SignalWirer  = True
isSameMode _            _            = False

data ToolModeUniversalState = ToolModeUniversalState
    { lastPlacedTilePos     :: !(Maybe (V2 Int))
    , selectedTile          :: !(Tile ImageID)
    , lastUndoRedoTime      :: !Word32
    , canEditEntityInstance :: !Bool
    }

instance Semigroup ToolModeUniversalState where (<>) = mappend
instance Monoid ToolModeUniversalState where mempty = ToolModeUniversalState Nothing emptyTile 0 True
instance Component ToolModeUniversalState where type Storage ToolModeUniversalState = Global ToolModeUniversalState
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.World.Tile where

import Control.Lens
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Kathu.Entity.Components (Render)
import qualified Kathu.Entity.Resource as R
import Kathu.Graphics.Drawable

newtype TileID = TileID Word16 deriving (Show, Eq)

newtype ToolType = ToolType Int deriving (Show, Eq)

data BreakBehavior =
      Breakable {_toolType :: ToolType, _minimumPower :: Double, _durability :: R.Dynamic Double}
    | Unbreakable
      deriving (Show, Eq, Generic)
makeLenses ''BreakBehavior

data Tile = Tile
    { _tileID :: TileID
    , _tileTextID :: Text
    , _tileName :: Text
    , _tileRender :: Render
    , _isSolid :: Bool
    , _breakBehavior :: BreakBehavior
    }
makeLenses ''Tile

emptyTileID :: TileID
emptyTileID = TileID 0

emptyTile = Tile
    { _tileID = emptyTileID
    , _tileTextID = ""
    , _tileName = ""
    , _tileRender = error "Attempted to draw an empty tile"
    , _isSolid = False
    , _breakBehavior = Unbreakable
    }

mkTileState :: Tile -> TileState
mkTileState t = TileState t 0 0

data TileState = TileState
    { _tile :: !Tile
    , _surroundingTiles :: !Word8
    , _metadata :: !Word8
    }
makeLenses ''TileState
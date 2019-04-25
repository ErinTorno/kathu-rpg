{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.World.Tile where

import Control.Lens
import Data.Text
import Data.Word
import GHC.Generics
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
    , _tileRender :: RenderSprite
    , _isSolid :: Bool
    , _breakBehavior :: BreakBehavior
    }
makeLenses ''Tile

emptyTile = Tile
    { _tileID = TileID 0
    , _tileTextID = ""
    , _tileName = ""
    , _tileRender = error "Attempted to draw an empty tile"
    , _isSolid = False
    , _breakBehavior = Unbreakable
    }

mkTileState :: Tile -> TileState
mkTileState = (flip TileState) 0

data TileState = TileState
    { _tile :: Tile
    , _metadata :: Word16
    }
makeLenses ''TileState
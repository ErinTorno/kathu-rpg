{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kathu.World.Tile where

import Data.Text
import Data.Word
import GHC.Generics
import qualified Kathu.Entity.Resource as R

newtype TileID = TileID Word16 deriving (Show, Eq)

newtype ToolType = ToolType Int deriving (Show, Eq)

data BreakBehavior =
      Breakable {toolType :: ToolType, minimumPower :: Double, durability :: R.Dynamic Double}
    | Unbreakable
      deriving (Show, Eq, Generic)

data Tile = Tile
    { tileID :: TileID
    , tileTextID :: Text
    , tileName :: Text
    -- Drawable
    , breakBehavior :: BreakBehavior
    } deriving (Show, Eq, Generic)

emptyTile = Tile
    { tileID = TileID 0
    , tileTextID = ""
    , tileName = ""
    , breakBehavior = Unbreakable
    }
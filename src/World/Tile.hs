{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module World.Tile where

import Data.Text
import Data.Word
import GHC.Generics

newtype TileID = TileID Word16 deriving (Show, Eq)

newtype ToolType = ToolType Int deriving (Show, Eq)

data BreakBehavior =
      Breakable {toolType :: ToolType, minimumPower :: Double, durability :: Double}
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
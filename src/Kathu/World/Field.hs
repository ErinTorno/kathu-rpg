{-# TemplateHaskell #-}

module Kathu.World.Field where

import Control.Lens
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import Kathu.World.Tile

fieldSize :: Num a => a
fieldSize = 32

newtype Field = Field (IOVector TileState)

mkField :: () -> IO Field
mkField () = fmap Field . MVec.replicate (fieldSize * fieldSize) . mkTileState $ emptyTile

getTileState :: (Int, Int) -> Field -> IO TileState
getTileState (x, y) (Field st) = MVec.read st (x * fieldSize + y)
module Kathu.World.Field where

import Control.Lens
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import Kathu.Graphics.Drawable
import Kathu.World.Tile
import Linear.V2 (V2(..))

fieldSize :: Num a => a
fieldSize = 32

data Field = Field
    { fieldCoord :: V2 Int
    , fieldData :: (IOVector TileState)
    --, fieldIndRenders :: (IOVector Bool)
    --, fieldPreRenders :: [Image]
    }

mkField :: V2 Int -> IO Field
mkField coord = fmap (Field coord) . MVec.replicate size . mkTileState $ emptyTile
    where size = fieldSize * fieldSize

getTileState :: V2 Int -> Field -> IO TileState
getTileState (V2 x y) (Field _ st) = MVec.read st (x * fieldSize + y)
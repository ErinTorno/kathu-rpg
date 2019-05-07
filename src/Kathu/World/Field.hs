module Kathu.World.Field where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import Kathu.Graphics.Drawable
import Kathu.World.Tile
import Linear.V3 (V3(..))

-- Fields are 32 by 32 tiles
fieldSize :: Num a => a
fieldSize = 32

-- Fields have a height of 4 tiles tall
fieldHeight :: Num a => a
fieldHeight = 4

data Field = Field
    { fieldCoord :: V3 Int
    , fieldData :: (IOVector TileState)
    --, fieldIndRenders :: (IOVector Bool)
    --, fieldPreRenders :: [Image]
    }

mkField :: MonadIO m => V3 Int -> m Field
mkField coord = liftIO . fmap (Field coord) . MVec.replicate size . mkTileState $ emptyTile
    where size = fieldSize * fieldSize * fieldHeight

fetchTileState :: MonadIO m => V3 Int -> Field -> m TileState
fetchTileState (V3 x y z) (Field _ tilest) = liftIO $ MVec.read tilest (z * (fieldSize * fieldSize) + x * fieldSize + y)

foreachTile :: MonadIO m => (TileState -> m a) -> Field -> m ()
foreachTile f (Field _ tilest) = go 0
    where len = MVec.length tilest
          go i | i == len  = pure ()
               | otherwise = liftIO (MVec.read tilest i) >>= f >> go (i + 1)
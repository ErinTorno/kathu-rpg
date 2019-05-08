module Kathu.World.Field where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vec
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

type FieldSet = Map (V3 Int) Field

newtype Field = Field
    { fieldData :: (IOVector TileState)
    --, fieldIndRenders :: (IOVector Bool)
    --, fieldPreRenders :: [Image]
    }

mkField :: MonadIO m => m Field
mkField = liftIO . fmap Field . MVec.replicate size . mkTileState $ emptyTile
    where size = fieldSize * fieldSize * fieldHeight

fetchTileState :: MonadIO m => V3 Int -> Field -> m TileState
fetchTileState (V3 x y z) (Field tilest) = liftIO $ MVec.read tilest (z * (fieldSize * fieldSize) + x * fieldSize + y)

foreachTile :: MonadIO m => (TileState -> m a) -> Field -> m ()
foreachTile f (Field tilest) = go 0
    where len = MVec.length tilest
          go i | i == len  = pure ()
               | otherwise = liftIO (MVec.read tilest i) >>= f >> go (i + 1)

-- Conversion

{-

mkFields :: Int -> Int -> Int -> m FieldSet
mkField x y z = replicateM z (replicateM y (replicateM z mkField))
    where toPair :: [[[Field]]] -> [(V3, Field)]
    
    
fromTileList :: MonadIO m => Vector (Vector (Vector (Tile))) -> m FieldSet
fromTileList vec = if Vec.null vec || Vec.null (vec Vec.! 0) || Vec.null ((vec Vec.! 0) Vec.! 0) then empt else build
    where minFields s = ceiling . (/s) . fromIntegral
          xLayers = minFields fieldSize $ Vec.length ((vec Vec.! 0) Vec.! 0)
          yLayers = minFields fieldSize $ Vec.length (vec Vec.! 0)
          zLayers = minFields fieldHeight $ Vec.length vec 
          mkLayer :: IOVector TileState -> Vector (Vector (Tile)) ->
          mkLayer
          empt = mkField 
          build = replicateM zLayers (replicateM xLayers (replicateM yLayers mkField))
-}
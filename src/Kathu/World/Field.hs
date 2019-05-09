{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Kathu.World.Field where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import Kathu.Graphics.Drawable
import Kathu.Util.MultiDimVector
import Kathu.World.Tile
import Linear.V3 (V3(..))

unitsPerTile :: Num a => a
unitsPerTile = 16

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

indexFromCoord :: V3 Int -> Int
indexFromCoord (V3 x y z) = z * (fieldSize * fieldSize) + x * fieldSize + y

fieldContainingCoord :: RealFrac a => V3 a -> V3 Int
fieldContainingCoord (V3 x y z) = floor <$> (V3 (x / (unitsPerTile * fieldSize)) (y / (unitsPerTile * fieldSize)) (z / (unitsPerTile * fieldHeight)))

-- Field Coord -> Local Coord in Field -> World Coord
worldCoordFromTile :: Num a => V3 Int -> V3 Int -> V3 a
worldCoordFromTile (V3 fx fy fz) (V3 lx ly lz) = V3 (conv fieldSize fx lx) (conv fieldSize fy ly) (conv fieldHeight fz lz)
    where conv s f l = unitsPerTile * fromIntegral ((f * s) + l)

fetchTileState :: MonadIO m => V3 Int -> Field -> m TileState
fetchTileState v (Field tilest) = liftIO $ MVec.read tilest (indexFromCoord v)

setTileState :: MonadIO m => V3 Int -> TileState -> Field -> m ()
setTileState v t (Field tilest) = liftIO $ MVec.write tilest (indexFromCoord v) t

foreachTile :: MonadIO m => (TileState -> m a) -> Field -> m ()
foreachTile f (Field tilest) = go 0
    where len = MVec.length tilest
          go i | i == len  = pure ()
               | otherwise = liftIO (MVec.read tilest i) >>= f >> go (i + 1)

fieldFoldM :: MonadIO m => (a -> V3 Int -> TileState -> m a) -> a -> Field -> m a
fieldFoldM f acc (Field tiles) = go 0 0 0 acc
    where go !x !y !z !b | z == fieldHeight = pure b
                         | y == fieldSize   = go 0 0 (z + 1) b
                         | x == fieldSize   = go 0 (y + 1) z b
                         | otherwise        = let v = V3 x y z in liftIO (MVec.read tiles (indexFromCoord v)) >>= f b v >>= go (x + 1) y z

-- Conversion


mkFields :: MonadIO m => Int -> Int -> Int -> m (Vector3D Field)
mkFields x y z = repliVecM z (repliVecM y (repliVecM z mkField))
    where repliVecM :: MonadIO m => Int -> m a -> m (Vector a)
          repliVecM n = fmap Vec.fromList . replicateM n
   
fromTileList :: MonadIO m => Vector3D Tile -> m FieldSet
fromTileList tiles = if zLayers == 0 || yLayers == 0 || xLayers == 0 then empty else buildNew 
    where empty                       = Map.singleton (V3 0 0 0) <$> mkField
          minFields s                 = ceiling . (/s) . fromIntegral
          getContField x y z = fieldContainingCoord $ fromIntegral <$> (V3 x y z)
          -- tiles are stored as z x y, rather than x y z
          (zLen, yLen, xLen)          = (Vec.length tiles, Vec.length (tiles Vec.! 0), Vec.length ((tiles Vec.! 0) Vec.! 0))
          (zLayers, yLayers, xLayers) = (minFields fieldHeight zLen, minFields fieldSize yLen, minFields fieldSize xLen)
          buildNew = liftIO (new3DWith xLayers yLayers zLayers Nothing) >>= go 0 0 0 >>= convertToMap
          go :: MonadIO m => Int -> Int -> Int -> IOVector3D (Maybe Field) -> m (IOVector3D (Maybe Field))
          go x y z v | z == zLen = pure v
                     | y == yLen = go 0 0 (z + 1) v
                     | x == xLen = go 0 (y + 1) z v
                     | otherwise = (runForTile (read3D z y x tiles) writeTile) >> go (x + 1) y z v
              where (V3 fx fy fz) = getContField x y z
                    runForTile :: MonadIO m => Tile -> (Tile -> Field -> m ()) -> m ()
                    runForTile t f | t ^. tileID == emptyTileID = pure () -- do nothing if wanting to right empty tile
                                   | otherwise = liftIO (mRead3D fx fy fz v) >>= \case
                                       Just (m) -> f t m
                                       Nothing  -> mkField >>= \field -> liftIO (mWrite3D fx fy fz (Just field) v) >> f t field
                    writeTile :: MonadIO m => Tile -> Field -> m ()
                    writeTile t field = setTileState (V3 x y z) (mkTileState t) field
          convertToMap :: MonadIO m => IOVector3D (Maybe Field) -> m FieldSet
          convertToMap iovec = fmap Map.fromList . liftIO .  miFoldl3D foldf [] $ iovec
              where foldf acc x y z cur = pure $ maybe acc ((:acc) . (V3 x y z,)) cur
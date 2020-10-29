module Verda.System.Tile.Chunks where

import           Apecs                        hiding (Map)
import           Control.Lens
import           Control.Monad.IO.Class       (MonadIO)
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable  as UMVec
import           Linear.V2

import           Verda.System.Tile.Components
import           Verda.Util.Apecs

unitsPerTile :: Num a => a
unitsPerTile = 1

chunkDim :: Num a => a
chunkDim = 32

mkEmptyChunk :: MonadIO m => m Chunk
mkEmptyChunk = liftIO $ Chunk <$> UMVec.replicate size emptyTileState
    where size    = chunkDim * chunkDim

chunksSurrounding :: RealFrac a => a -> a -> Chunks -> [(V2 Int, Chunk)]
chunksSurrounding wx wy (Chunks chunkMap) = catMaybes $ readChunks [] (ox - 1) (oy - 1)
    where V2 ox oy = chunkContainingCoordF wx wy
          readChunks !acc !x !y | y > oy + 1 = acc
                                | x > ox + 1 = readChunks acc (ox - 1) (y + 1)
                                | otherwise  = readChunks (((curV,) <$> Map.lookup curV chunkMap):acc) (x + 1) y
                                where curV = V2 x y

mkChunkIfNotPresent :: (ReadWrite w m Chunks, MonadIO m) => V2 Int -> SystemT w m Chunk
mkChunkIfNotPresent coord = do
    Chunks chmap <- get global
    case Map.lookup coord chmap of
        Just c  -> pure c
        Nothing -> do
            newChunk <- mkEmptyChunk
            global   $= Chunks (Map.insert coord newChunk chmap)
            pure newChunk

getTileStateFromChunks :: MonadIO m => V2 Int -> Chunks -> m TileState
getTileStateFromChunks !coord (Chunks !chunkMap) =
    let V2 locX locY = localCoordFromGlobal coord
     in case Map.lookup (chunkContainingCoord coord) chunkMap of
        Nothing -> pure emptyTileState
        Just c  -> getTileState locX locY c

getChunksTileState :: (Has w m Chunks, MonadIO m) => V2 Int -> SystemT w m TileState
getChunksTileState !coord = do
    Chunks chunkMap <- get global
    let V2 locX locY = localCoordFromGlobal coord
    case Map.lookup (chunkContainingCoord coord) chunkMap of
        Nothing -> pure emptyTileState
        Just c  -> getTileState locX locY c

setChunksTileState :: (ReadWrite w m Chunks, MonadIO m) => V2 Int -> TileState -> SystemT w m ()
setChunksTileState coord tileSt = do
    let chunkCoord   = chunkContainingCoord coord
        V2 locX locY = localCoordFromGlobal coord
    chunk <- mkChunkIfNotPresent chunkCoord
    setTileState chunk locX locY tileSt

--------------
-- Indexing --
--------------

{-# INLINE indexFromCoord #-}
indexFromCoord :: Int -> Int -> Int
indexFromCoord !x !y = y * chunkDim + x

chunkContainingCoordF :: RealFrac a => a -> a -> V2 Int
chunkContainingCoordF !x !y = V2 (getCoord x) (getCoord y)
    where getCoord = floor . (/(unitsPerTile * chunkDim))

chunkContainingCoord :: V2 Int -> V2 Int
chunkContainingCoord (V2 x y) = V2 (getCoord x) (getCoord y)
    where getCoord = (`div`(unitsPerTile * chunkDim))

setTileState :: MonadIO m => Chunk -> Int -> Int -> TileState -> m ()
setTileState (Chunk tiles) !x !y t = liftIO $ UMVec.write tiles (indexFromCoord x y) t

getTileState :: MonadIO m => Int -> Int -> Chunk -> m TileState
getTileState !x !y (Chunk tiles) = liftIO $ UMVec.read tiles (indexFromCoord x y)

-- Chunk Coord -> Local Coord in Field -> World Coord
worldCoordFromTileCoord :: Num a => V2 Int -> V2 Int -> V2 a
worldCoordFromTileCoord (V2 !fx !fy) (V2 !lx !ly) = V2 (conv chunkDim fx lx) (conv chunkDim fy ly)
    where conv !s !f !l = unitsPerTile * fromIntegral ((f * s) + l)


localCoordFromGlobal :: V2 Int -> V2 Int
localCoordFromGlobal pos = pos - fieldPos
    where fieldPos = (*chunkDim) . (`div`chunkDim) <$> pos

---------------
-- Iterating --
---------------

{-# INLINE foldNonEmptyM #-}
-- | Folds through all present tiles in the chunk monadically, with position; skips empty tiles
foldNonEmptyM :: MonadIO m => (a -> V2 Int -> TileState -> m a) -> a -> Chunk -> m a
foldNonEmptyM f !acc (Chunk chunkTiles) = go chunkTiles 0 0 acc
    where go tiles !x !y !b
              | y == chunkDim = pure b
              | x == chunkDim = go tiles 0 (y + 1) b
              | otherwise     = liftIO (UMVec.unsafeRead tiles $ indexFromCoord x y)
                            >>= ignoreEmpty b (f b (V2 x y))
                            >>= go tiles (x + 1) y
          ignoreEmpty b action !t = if t^.tsTileID == emptyTileID then pure b else action t

{-# INLINE foldAllM #-}
-- | Folds through all present tiles in the chunk monadically, with position; skips empty tiles
foldAllM :: MonadIO m => (a -> V2 Int -> TileState -> m a) -> a -> Chunk -> m a
foldAllM f !acc (Chunk chunkTiles) = go chunkTiles 0 0 acc
    where go tiles !x !y !b
              | y == chunkDim = pure b
              | x == chunkDim = go tiles 0 (y + 1) b
              | otherwise     = liftIO (UMVec.unsafeRead tiles $ indexFromCoord x y)
                            >>= f b (V2 x y)
                            >>= go tiles (x + 1) y

isChunkNonEmpty :: MonadIO m => Chunk -> m Bool
isChunkNonEmpty = foldNonEmptyM (\_ _ _ -> pure True) False
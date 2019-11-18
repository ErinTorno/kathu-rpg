{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module Kathu.World.Field where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.ST            (RealWorld)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (maybe)
import qualified Data.Vector                 as Vec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Linear.V2                   (V2(..))

import           Kathu.Util.MultiDimVector
import           Kathu.World.Tile

unitsPerTile :: Num a => a
unitsPerTile = 1

-- Fields are 32 by 32 tiles
fieldDim :: Num a => a
fieldDim = 32

type FieldSet = Map (V2 Int) Field

newtype Field = Field {unField :: MVector RealWorld TileState}
    --, fieldIndRenders :: (IOVector Bool)
    --, fieldPreRenders :: [Image]

mkField :: MonadIO m => m (Field)
mkField = liftIO $ Field <$> UMVec.replicate size emptyTS
    where size    = fieldDim * fieldDim
          emptyTS = mkTileState emptyTile

--------------------------
-- Int -> Int Functions --
--------------------------

{-# INLINE indexFromCoord #-}
indexFromCoord :: Int -> Int -> Int
indexFromCoord !x !y = y * fieldDim + x

{-# INLINE coordFromIndex #-}
coordFromIndex :: Int -> (Int, Int)
coordFromIndex p = let (y, x) = p `quotRem` fieldDim in (x, y)

fieldContainingCoord :: RealFrac a => a -> a -> (# Int, Int #)
fieldContainingCoord !x !y = (# getCoord x, getCoord y #)
    where getCoord = floor . (/(unitsPerTile * fieldDim))

fetchTileState :: MonadIO m => Int -> Int -> Field -> m (TileState)
fetchTileState !x !y (Field fgTiles) = liftIO $ UMVec.read fgTiles (indexFromCoord x y)

setTileState :: MonadIO m => Int -> Int -> TileState -> Field -> m ()
setTileState !x !y t (Field fgTiles) = liftIO $ UMVec.write fgTiles (indexFromCoord x y) t

----------------------
-- V2 Int Functions --
----------------------

{-# INLINE indexFromCoordV2 #-}
indexFromCoordV2 :: V2 Int -> Int
indexFromCoordV2 (V2 !x !y) = x * fieldDim + y

fieldContainingCoordV2 :: RealFrac a => V2 a -> (# Int, Int #)
fieldContainingCoordV2 (V2 !x !y) = (# getCoord x, getCoord y #)
    where getCoord = floor . (/(unitsPerTile * fieldDim))

fetchTileStateV2 :: MonadIO m => V2 Int -> Field -> m (TileState)
fetchTileStateV2 (V2 !x !y) (Field fgTiles) = liftIO $ UMVec.read fgTiles (indexFromCoord x y)

setTileStateV2 :: MonadIO m => V2 Int -> TileState -> Field -> m ()
setTileStateV2 (V2 !x !y) t (Field fgTiles) = liftIO $ UMVec.write fgTiles (indexFromCoord x y) t

-- Field Coord -> Local Coord in Field -> World Coord
worldCoordFromTileCoord :: Num a => V2 Int -> V2 Int -> V2 a
worldCoordFromTileCoord (V2 !fx !fy) (V2 !lx !ly) = V2 (conv fieldDim fx lx) (conv fieldDim fy ly)
    where conv !s !f !l = unitsPerTile * fromIntegral ((f * s) + l)

----------
-- Misc --
----------

foreachTile :: MonadIO m => (TileState -> m a) -> Field -> m ()
foreachTile f (Field fgTiles) = go fgTiles 0
    where len = UMVec.length fgTiles
          go tiles !i | i == len  = pure ()
                      | otherwise = liftIO (UMVec.unsafeRead tiles i) >>= f >> go tiles (i + 1)

{-# INLINE foreachPosTile #-}
-- | Folds through all present tiles in the field monadically, with position; skips empty tiles
foreachPosTile :: MonadIO m => (V2 Int -> TileState -> m a) -> Field -> m ()
foreachPosTile f (Field fgTiles) = go fgTiles 0 0
    where go tiles !x !y
              | y == fieldDim   = pure ()
              | x == fieldDim   = go tiles 0 (y + 1)
              | otherwise       = liftIO (UMVec.unsafeRead tiles $ indexFromCoord x y) >>= ignoreEmpty (f (V2 x y)) >> go tiles (x + 1) y
          ignoreEmpty action !t = when (t^.tile /= emptyTileID) $ void (action t)

{-# INLINE fieldFoldM #-}
-- | Folds through all present tiles in the field monadically, with position; skips empty tiles
fieldFoldM :: MonadIO m => (a -> V2 Int -> TileState -> m a) -> a -> Field -> m a
fieldFoldM f !acc (Field fgTiles) = go fgTiles 0 0 acc
    where go tiles !x !y !b
              | y == fieldDim     = pure b
              | x == fieldDim     = go tiles 0 (y + 1) b
              | otherwise         = liftIO (UMVec.unsafeRead tiles $ indexFromCoord x y) >>= ignoreEmpty b (f b (V2 x y)) >>= go tiles (x + 1) y
          ignoreEmpty b action !t = if t^.tile == emptyTileID then pure b else action t

----------------
-- Conversion --
----------------

mkFields :: MonadIO m => Int -> Int -> m (Vector2D (Field))
mkFields x y = repliVecM x (repliVecM y mkField)
    where repliVecM n = fmap Vec.fromList . replicateM n
   
-- | Creates a FieldSet from a given 2D Vector of Tiles 
fromTileVector2D :: MonadIO m => Vector2D (Tile g) -> m FieldSet
fromTileVector2D fgTiles = if yLayers == 0 || xLayers == 0 then empty else buildNew 
    where empty                       = Map.singleton (V2 0 0) <$> mkField
          minFields :: (Integral a) => Float -> a -> Int
          minFields s                 = ceiling . (/s) . fromIntegral
          getContField x y   = fieldContainingCoord ((fromIntegral :: Int -> Double) x) (fromIntegral y)
          -- tiles are stored as x y, rather than x y
          (yLen, xLen)       = (Vec.length fgTiles, Vec.length (fgTiles Vec.! 0))
          (yLayers, xLayers) = (minFields fieldDim yLen, minFields fieldDim xLen)
          buildNew = liftIO (new2DWith xLayers yLayers Nothing) >>= go fgTiles 0 0 >>= convertToMap
          go :: MonadIO m => Vector2D (Tile g) -> Int -> Int -> IOVector2D (Maybe Field) -> m (IOVector2D (Maybe Field))
          go !tiles !x !y v | y == yLen = pure v
                                   | x == xLen = go tiles 0 (y + 1) v
                                   | otherwise = (runForTile (read2D y x tiles) writeTile) >> go tiles (x + 1) y v
              where (# fx, fy #) = getContField x y
                    runForTile t f | t^.tileID == emptyTileID = pure () -- do nothing if wanting to write empty tile, since Fields are initialized with all emptyTile
                                   | otherwise                = liftIO (mRead2D fx fy v) >>= \case
                                       Just (m) -> f t m
                                       Nothing  -> mkField >>= \field -> liftIO (mWrite2D fx fy (Just field) v) >> f t field
                    writeTile t field = mkTileStateWithMetadata t >>= \newTileState -> setTileState x y newTileState field
          convertToMap iovec = fmap Map.fromList . liftIO .  miFoldl2D foldf [] $ iovec
              where foldf acc x y cur = pure $ maybe acc ((:acc) . (V2 x y,)) cur
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module Kathu.World.Field where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.ST            (RealWorld)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Vector                 as Vec
import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Linear.V2                   (V2(..))

import           Kathu.Entity.System         (Tiles, fromTiles)
import           Kathu.Util.Flow             (mapPair, mapFst, mapSnd)
import           Kathu.Util.Polygon
import           Kathu.World.Tile

unitsPerTile :: Num a => a
unitsPerTile = 1

-- Fields are 32 by 32 tiles
fieldDim :: Num a => a
fieldDim = 32

type FieldSet = Map (V2 Int) Field

newtype Field = Field {unField :: MVector RealWorld TileState}

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


{-# INLINE fieldFoldWithEmptyM #-}
-- | Folds through all present tiles in the field monadically, with position; skips empty tiles
fieldFoldWithEmptyM :: MonadIO m => (a -> V2 Int -> TileState -> m a) -> a -> Field -> m a
fieldFoldWithEmptyM f !acc (Field fgTiles) = go fgTiles 0 0 acc
    where go tiles !x !y !b
              | y == fieldDim     = pure b
              | x == fieldDim     = go tiles 0 (y + 1) b
              | otherwise         = liftIO (UMVec.unsafeRead tiles $ indexFromCoord x y) >>= f b (V2 x y) >>= go tiles (x + 1) y

----------------
-- Conversion --
----------------
   
-- | Creates a FieldSet from a given 2D Vector of Tiles 
fromTileVector2D :: MonadIO m => Vec.Vector (Vec.Vector (Tile g)) -> m FieldSet
fromTileVector2D fgTiles = if yLayers == 0 || xLayers == 0 then empty else buildNew
    where empty                       = Map.singleton (V2 0 0) <$> mkField
          minFields :: (Integral a) => Float -> a -> Int
          minFields s                 = ceiling . (/s) . fromIntegral
          getContField x y   = fieldContainingCoord ((fromIntegral :: Int -> Double) x) (fromIntegral y)
          (yLen, xLen)       = (Vec.length fgTiles, Vec.length (fgTiles Vec.! 0))
          (yLayers, xLayers) = (minFields fieldDim yLen, minFields fieldDim xLen)
          buildNew = liftIO (go 0 0 Map.empty)
          go :: Int -> Int -> Map (V2 Int) Field -> IO (Map (V2 Int) Field)
          go !x !y m | y == yLen = pure m
                     | x == xLen = go 0 (y + 1) m
                     | otherwise = runForTile ((fgTiles Vec.! y) Vec.! x) writeTile >>= go (x + 1) y
              where (# fx, fy #) = getContField x y
                    runForTile :: Tile g -> (Tile g -> Field -> IO Field) -> IO (Map (V2 Int) Field)
                    runForTile t a | t^.tileID == emptyTileID = pure m -- do nothing if wanting to write empty tile, since Fields are initialized with all emptyTile
                                   | otherwise                = case Map.lookup (V2 fx fy) m of
                                       Just f  -> a t f >> pure m
                                       Nothing -> ((flip (Map.insert (V2 fx fy))) m) <$> (a t =<< mkField)
                    writeTile :: Tile g -> Field -> IO Field
                    writeTile t field = do
                        newTileState <- mkTileStateWithMetadata t
                        setTileState (x - fx * fieldDim) (y - fy * fieldDim) newTileState field
                        pure field
          --debugPrint fs = (liftIO . putStrLn . concat $ ["x-len ", show xLen, "; y-len ", show yLen, "; x-layers ", show xLayers, "; y-layers ", show yLayers]) >> pure fs

-- | Transforms a FieldSet into a list of V2 lists defining collision shapes
mkCollisionPolygons :: MonadIO m => Tiles g -> FieldSet -> m (Vec.Vector [V2 Double])
mkCollisionPolygons tiles = fmap (Vec.concat . fmap mkTriangles) . mapM isSolidVec . Map.assocs
    where isSolidTS :: MonadIO m => TileState -> m Bool
          isSolidTS ts = view isSolid <$> (liftIO . fromTiles tiles $ ts)
          -- transforms field into a 1D vector of bools for if is solid; accessed at pos with indexFromCoord
          isSolidVec :: MonadIO m => (V2 Int, Field) -> m (V2 Int, UVec.Vector Bool)
          isSolidVec (pos, f) = fmap ((pos,) . UVec.fromList . reverse) . fieldFoldWithEmptyM (\acc _ ts -> (:acc) <$> isSolidTS ts) [] $ f
          
          mkTriangles :: (V2 Int, UVec.Vector Bool) -> Vec.Vector [V2 Double]
          mkTriangles ((V2 wx wy), v) = uncurry (Vec.++)
                                      . mapFst  (Vec.map polyBorder)
                                      . mapSnd  (Vec.concat . Vec.toList . Vec.map (Vec.fromList . triangulate))
                                      . mapPair (Vec.map (mapPolyVertices ((+(V2 (-0.5) (-1))) . fmap fromIntegral . (+(V2 (wx * fieldDim) (wy * fieldDim))))))
                                      $ convexAndConcaveFromBinaryGrid v fieldDim fieldDim
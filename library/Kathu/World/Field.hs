{-# LANGUAGE UnboxedTuples     #-}

module Kathu.World.Field where

import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.ST            (RealWorld)
import           Data.Aeson
import           Data.Aeson.Types            (Pair)
import           Data.Bifunctor              as Bi
import qualified Data.Foldable               as F
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Data.Vector                 as Vec
import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Linear.V2                   (V2(..))

import           Kathu.Entity.System         (Tiles, fromTiles, fromTilesID)
import           Kathu.Parsing.Aeson         ()
import           Kathu.Util.Containers       (foldlMVec, foldrMVec, fromJustElseError, splitEveryN)
import           Kathu.Util.Polygon
import           Kathu.Util.Types
import           Kathu.World.Tile

unitsPerTile :: Num a => a
unitsPerTile = 1

-- Fields are 32 by 32 tiles
fieldDim :: Num a => a
fieldDim = 32

newtype FieldSet = FieldSet {unFieldSet :: Map (V2 Int) Field}

emptyFieldSet :: FieldSet
emptyFieldSet = FieldSet Map.empty

newtype Field = Field {unField :: MVector RealWorld TileState}

mkField :: MonadIO m => m Field
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

fetchTileState :: MonadIO m => Int -> Int -> Field -> m TileState
fetchTileState !x !y (Field fgTiles) = liftIO $ UMVec.read fgTiles (indexFromCoord x y)

setTileState :: MonadIO m => Int -> Int -> TileState -> Field -> m ()
setTileState !x !y t (Field fgTiles) = liftIO $ UMVec.write fgTiles (indexFromCoord x y) t

----------------------
-- V2 Int Functions --
----------------------

{-# INLINE indexFromCoordV2 #-}
indexFromCoordV2 :: V2 Int -> Int
indexFromCoordV2 (V2 !x !y) = x * fieldDim + y

-- | Takes a coordinate from the entire world, and transforms it into the coordinate relative to the containing field
localCoordFromGlobalV2 :: V2 Int -> V2 Int
localCoordFromGlobalV2 pos = pos - fieldPos
    where fieldPos = (*fieldDim) . (`div`fieldDim) <$> pos

fieldContainingCoordV2 :: RealFrac a => V2 a -> V2 Int
fieldContainingCoordV2 (V2 !x !y) = V2 (getCoord x) (getCoord y)
    where getCoord = floor . (/(unitsPerTile * fieldDim))

fetchTileStateV2 :: MonadIO m => V2 Int -> Field -> m TileState
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
-- | Folds through all present tiles in the field monadically, with position
fieldFoldWithEmptyM :: MonadIO m => (a -> V2 Int -> TileState -> m a) -> a -> Field -> m a
fieldFoldWithEmptyM f !acc (Field fgTiles) = go fgTiles 0 0 acc
    where go tiles !x !y !b
              | y == fieldDim     = pure b
              | x == fieldDim     = go tiles 0 (y + 1) b
              | otherwise         = liftIO (UMVec.unsafeRead tiles $ indexFromCoord x y) >>= f b (V2 x y) >>= go tiles (x + 1) y

isFieldEmpty :: MonadIO m => Field -> m Bool
isFieldEmpty = fieldFoldM (\_ _ _ -> pure False) True

isFieldNotEmpty :: MonadIO m => Field -> m Bool
isFieldNotEmpty = fieldFoldM (\_ _ _ -> pure True) False

-- | Transforms a FieldSet into a list of V2 lists defining collision shapes
mkCollisionPolygons :: MonadIO m => Tiles g -> FieldSet -> m (Vec.Vector [V2 Double])
mkCollisionPolygons tiles = fmap (Vec.concat . fmap mkTriangles) . mapM isSolidVec . Map.assocs . unFieldSet
    where isSolidTS :: MonadIO m => TileState -> m Bool
          isSolidTS ts = view isSolid <$> (liftIO . fromTiles tiles $ ts)
          -- transforms field into a 1D vector of bools for if is solid; accessed at pos with indexFromCoord
          isSolidVec :: MonadIO m => (V2 Int, Field) -> m (V2 Int, UVec.Vector Bool)
          isSolidVec (pos, f) = fmap ((pos,) . UVec.fromList . reverse) . fieldFoldWithEmptyM (\acc _ ts -> (:acc) <$> isSolidTS ts) [] $ f
          
          mapPair f (a, b) = (f a, f b)

          mkTriangles :: (V2 Int, UVec.Vector Bool) -> Vec.Vector [V2 Double]
          mkTriangles (V2 wx wy, v) = uncurry (Vec.++)
                                    . Bi.first  (Vec.map polyBorder)
                                    . Bi.second (Vec.concat . Vec.toList . Vec.map (Vec.fromList . triangulate))
                                    . mapPair (Vec.map (mapPolyVertices ((+ V2 (-0.5) (-1)) . fmap fromIntegral . (+ V2 (wx * fieldDim) (wy * fieldDim)))))
                                    $ convexAndConcaveFromBinaryGrid v fieldDim fieldDim

-------------------
-- Serialization --
-------------------

type MapLegend g = Map Char (Tile g)

data FieldConfig = FieldConfig {fcPosition :: !(V2 Int), fcData :: !(Vec.Vector String)}

instance ToJSON FieldConfig where
    toJSON (FieldConfig pos fdata) = object ["position" .= pos, "data" .= fdata]

instance FromJSON FieldConfig where
    parseJSON = withObject "FieldConfig" $ \v -> FieldConfig <$> v .: "position" <*> v .: "data"

applyFieldConfig :: MonadIO m => MapLegend g -> FieldSet -> FieldConfig -> m FieldSet
applyFieldConfig legend (FieldSet fieldMap) (FieldConfig pos fdata) = liftIO $ do
    Field field <- mkField

    let legendLookup k = mkTileStateWithMetadata
                       . fromJustElseError "Attempted to tile without a listing in the WorldSpace's legend"
                       . Map.lookup k
                       $ legend

    iforM_ fdata $ \y rowStr ->
        iforM_ rowStr $ \x key ->
            UMVec.write field (indexFromCoord x y) =<< legendLookup key

    pure . FieldSet . flip (Map.insert pos) fieldMap . Field $ field

-- When creating a tile legend, we choose symbols from associated groups;
-- Once one of those groups run out, they can grab from the extra
legendSolidTileKeys, legendNonSolidTileKeys, legendExtraTileKeys :: [Char]
legendSolidTileKeys    = "#@=$%&/()0123456789£¢¥§©" ++ ['A'..'B']
legendNonSolidTileKeys = ".,_-'`~+*:;<>!?¤°" ++ ['a'..'z']
legendExtraTileKeys    = ['¿'..]

mkLegend :: MonadIO m => Tiles g -> FieldSet -> m (MapLegend g)
mkLegend allTiles (FieldSet fieldMap) = mkMap <$> liftIO allUniqueTiles
    where mkMap = Map.fromList . view _1 . F.foldl' assignNext initialAcc

          initialAcc = ([(' ', emptyTile)], legendSolidTileKeys, legendNonSolidTileKeys, legendExtraTileKeys)
          -- next tile will get its legend key given
          assignNext (acc, solid, nonSolid, extra) cTile
              | cTile^.isSolid = extraIfNull solid    (tail solid) nonSolid
              | otherwise      = extraIfNull nonSolid solid        (tail nonSolid)
              where extraIfNull keys solKeys nonSolKeys
                        | null keys = ((head extra, cTile):acc, solid,   nonSolid,   tail extra)
                        | otherwise = ((head keys, cTile):acc,  solKeys, nonSolKeys, extra)
        
          allUniqueTiles   = mapM (fromTilesID allTiles) =<< allUniqueTileIDs
        
          allUniqueTileIDs = Set.elems . Set.unions <$> uniqueTileIDSets
          
          uniqueTileIDSets = mapM getUniqueTiles . Map.elems $ fieldMap

          getUniqueTiles (Field fvec) = foldlMVec addUnique Set.empty fvec
          addUnique acc ts
              | ts^.tile == emptyTileID = acc -- ignore empty
              | otherwise               = Set.insert (ts^.tile) acc

-- | Serializes a FieldSet into pairs with a legend and many fields
-- | The legend is map of characters to tile text IDs
-- | The fields are a fieldDim-element list of fieldDim-character Strings
serializeFieldSetPairs :: MonadIO m => Tiles g -> FieldSet -> m [Pair]
serializeFieldSetPairs allTiles fs@(FieldSet fieldMap) = do
    legend    <- mkLegend allTiles fs
    let nameLegend :: Map Char Identifier
        nameLegend = view tileTextID <$> legend
        revLegend :: Map TileID Char
        revLegend  = Map.fromList . map (\(k, v) -> (v, k)) . Map.assocs . fmap (view tileID) $ legend
        getKey ts  = revLegend Map.! (ts^.tile)

    nonEmptyFields <- filterM (isFieldNotEmpty . snd) . Map.assocs $ fieldMap

    fieldLists <- liftIO . forM nonEmptyFields $ \(pos, Field field) -> do
        fieldStr <- foldrMVec (\ts str -> getKey ts : str) "" field

        let rows = Vec.fromList . take fieldDim $ splitEveryN fieldDim fieldStr

        pure (FieldConfig pos rows)

    pure ["legend" .= nameLegend, "fields" .= fieldLists]
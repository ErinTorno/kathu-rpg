module Kathu.World.ChunkBuilder where

import           Control.Lens                hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class      (MonadIO, liftIO)
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
import           Verda.System.Tile.Chunks
import           Verda.System.Tile.Components

import           Verda.Parsing.Aeson         ()
import           Verda.Util.Containers       (foldlMVec, foldrMVec, fromJustElseError, splitEveryN)
import           Kathu.Util.Polygon
import           Verda.Util.Types
import           Kathu.World.Tile

---------------
-- Collision --
---------------

-- | Transforms Chunks into a list of V2 lists defining collision shapes
mkCollisionPolygons :: MonadIO m => AllTiles' -> Chunks -> m (Vec.Vector [V2 Double])
mkCollisionPolygons tiles = fmap (Vec.concat . fmap mkTriangles) . mapM isSolidVec . Map.assocs . unChunks
    where isSolidTS :: MonadIO m => TileState -> m Bool
          isSolidTS ts = view isSolid <$> (liftIO . fetchTile tiles . view tsTileID $ ts)
          -- transforms field into a 1D vector of bools for if is solid; accessed at pos with indexFromCoord
          isSolidVec :: MonadIO m => (V2 Int, Chunk) -> m (V2 Int, UVec.Vector Bool)
          isSolidVec (pos, f) = fmap ((pos,) . UVec.fromList . reverse) . foldAllM (\acc _ ts -> (:acc) <$> isSolidTS ts) [] $ f
          
          mapPair f (a, b) = (f a, f b)

          mkTriangles :: (V2 Int, UVec.Vector Bool) -> Vec.Vector [V2 Double]
          mkTriangles (V2 wx wy, v) = uncurry (Vec.++)
                                    . Bi.first  (Vec.map polyBorder)
                                    . Bi.second (Vec.concat . Vec.toList . Vec.map (Vec.fromList . triangulate))
                                    . mapPair (Vec.map (mapPolyVertices ((+ V2 (-0.5) (-1)) . fmap fromIntegral . (+ V2 (wx * chunkDim) (wy * chunkDim)))))
                                    $ convexAndConcaveFromBinaryGrid v chunkDim chunkDim

-------------------
-- Serialization --
-------------------

type MapLegend = Map Char Tile

data ChunkConfig = ChunkConfig {ccPosition :: !(V2 Int), ccData :: !(Vec.Vector String)}

instance ToJSON ChunkConfig where
    toJSON (ChunkConfig pos fdata) = object ["position" .= pos, "data" .= fdata]

instance FromJSON ChunkConfig where
    parseJSON = withObject "ChunkConfig" $ \v -> ChunkConfig <$> v .: "position" <*> v .: "data"

applyChunkConfig :: MonadIO m => MapLegend -> Chunks -> ChunkConfig -> m Chunks
applyChunkConfig legend (Chunks chunkMap) (ChunkConfig pos fdata) = liftIO $ do
    Chunk chunk <- mkEmptyChunk

    let legendLookup k = mkTileStateWithMetadata
                       . fromJustElseError "Attempted to tile without a listing in the WorldSpace's legend"
                       . Map.lookup k
                       $ legend

    iforM_ fdata $ \y rowStr ->
        iforM_ rowStr $ \x key ->
            UMVec.write chunk (indexFromCoord x y) =<< legendLookup key

    pure . Chunks . flip (Map.insert pos) chunkMap . Chunk $ chunk

-- When creating a tile legend, we choose symbols from associated groups;
-- Once one of those groups run out, they can grab from the extra
legendSolidTileKeys, legendNonSolidTileKeys, legendExtraTileKeys :: [Char]
legendSolidTileKeys    = "#@=$%&/()0123456789£¢¥§©" ++ ['A'..'B']
legendNonSolidTileKeys = ".,_-'`~+*:;<>!?¤°" ++ ['a'..'z']
legendExtraTileKeys    = ['¿'..]

mkLegend :: MonadIO m => AllTiles' -> Chunks -> m MapLegend
mkLegend allTiles (Chunks chunkMap) = mkMap <$> liftIO allUniqueTiles
    where mkMap = Map.fromList . view _1 . F.foldl' assignNext initialAcc

          initialAcc = ([(' ', emptyTile)], legendSolidTileKeys, legendNonSolidTileKeys, legendExtraTileKeys)
          -- next tile will get its legend key given
          assignNext (acc, solid, nonSolid, extra) cTile
              | cTile^.isSolid = extraIfNull solid    (tail solid) nonSolid
              | otherwise      = extraIfNull nonSolid solid        (tail nonSolid)
              where extraIfNull keys solKeys nonSolKeys
                        | null keys = ((head extra, cTile):acc, solid,   nonSolid,   tail extra)
                        | otherwise = ((head keys, cTile):acc,  solKeys, nonSolKeys, extra)
        
          allUniqueTiles   = mapM (fetchTile allTiles) =<< allUniqueTileIDs
        
          allUniqueTileIDs = Set.elems . Set.unions <$> uniqueTileIDSets
          
          uniqueTileIDSets = mapM getUniqueTiles . Map.elems $ chunkMap

          getUniqueTiles (Chunk cvec) = foldlMVec addUnique Set.empty cvec
          addUnique acc ts
              | ts^.tsTileID == emptyTileID = acc -- ignore empty
              | otherwise                   = Set.insert (ts^.tsTileID) acc

-- | Serializes Chunks into pairs with a legend and many fields
-- | The legend is map of characters to tile text IDs
-- | The fields are a chunkDim-element list of chunkDim-character Strings
serializeFieldSetPairs :: MonadIO m => AllTiles' -> Chunks -> m [Pair]
serializeFieldSetPairs allTiles chunks@(Chunks chunkMap) = do
    legend    <- mkLegend allTiles chunks
    let nameLegend :: Map Char Identifier
        nameLegend = view tileTextID <$> legend
        revLegend :: Map TileID Char
        revLegend  = Map.fromList . map (\(k, v) -> (v, k)) . Map.assocs . fmap (view tileID) $ legend
        getKey ts  = revLegend Map.! (ts^.tsTileID)

    nonEmptyChunks <- filterM (isChunkNonEmpty . snd) . Map.assocs $ chunkMap

    chunkList <- liftIO . forM nonEmptyChunks $ \(pos, Chunk chunk) -> do
        chunkStr <- foldrMVec (\ts str -> getKey ts : str) "" chunk

        let rows = Vec.fromList . take chunkDim $ splitEveryN chunkDim chunkStr

        pure (ChunkConfig pos rows)

    pure ["legend" .= nameLegend, "chunks" .= chunkList]
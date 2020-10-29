module Verda.System.Tile.Renderer where

import           Apecs
import           Control.Lens
import           Control.Monad                (foldM)
import           Data.Proxy
import qualified Data.Vector.Mutable          as MVec
import           Linear.V2

import           Verda.Graphics.Components
import           Verda.System.Tile.Components
import           Verda.System.Tile.Chunks
import           Verda.World
import           Verda.Util.Apecs
import           Verda.Util.Containers        (forMVec)

addTileRendererExtension :: (VerdaWorld w IO, ReadWriteEach w IO '[AllTiles t, Chunks], VerdaTile t) => Proxy t -> SystemT w IO ()
addTileRendererExtension proxy =
    addSpriteRenderExtension $ \dT renderSprite (V2 camX camY) _ initIdx -> do
        allTiles <- unAllTiles <$> get global
        -- since tile graphics information isn't stored as entities, we instead just grab all tiles and update their animations
        lift $ forMVec allTiles (updateTileAnimation dT)
        chunks   <- get global
        let getTile = MVec.read allTiles . unTileID . view tsTileID

            addTile :: V2 Int -> Int -> V2 Int -> TileState -> IO Int
            addTile !chunkPos !idx !pos !tileSt =
                let drawPos = worldCoordFromTileCoord chunkPos pos
                 in getTile tileSt >>= \t -> renderSprite idx (getTileSprite tileSt (t `asProxyTypeOf` proxy)) drawPos noTint

            foldChunk !idx (!chunkPos, !chunk) = foldNonEmptyM (addTile chunkPos) idx chunk
        lift . foldM foldChunk initIdx . chunksSurrounding camX camY $ chunks
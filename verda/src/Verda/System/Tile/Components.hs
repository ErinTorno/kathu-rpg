{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Verda.System.Tile.Components
    ( AllTiles(..)
    , Chunk(..)
    , Chunks(..)
    , TileID
    , TileState(..)
    , VerdaTile(..)
    , unTileID
    , tsTileID
    , tsMetadata
    , emptyChunks
    , emptyTileID
    , emptyTileState
    , fetchTile
    ) where

import           Apecs                        hiding (Map)
import           Control.Lens
import           Control.Monad.IO.Class       (MonadIO)
import           Data.Aeson
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Vector.Mutable          (IOVector)
import qualified Data.Vector.Mutable          as MVec
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as UMVec
import           Data.Word
import           Linear.V2

import           Verda.Graphics.Sprites       (Sprite)
import           Verda.Parsing.Counting
import           Verda.Util.Dependency

newtype TileID = TileID {unTileID :: Int} deriving (Show, Eq, Ord)
derivingUnbox "TileID"
    [t| TileID -> Int |]
    [| unTileID       |]
    [| TileID         |]

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m TileID) where
    parseJSON = parseAndLookupOrAddIncrementalID TileID "TileID"

emptyTileID :: TileID
emptyTileID = TileID (-1)

data TileState = TileState
    { _tsTileID   :: {-# UNPACK #-} !TileID
    , _tsMetadata :: {-# UNPACK #-} !Word64
    } deriving (Show, Eq)
makeLenses ''TileState
derivingUnbox "TileState"
    [t| TileState -> (TileID, Word64) |]
    [| \(TileState tl mt) -> (tl, mt) |]
    [| uncurry TileState              |]

emptyTileState :: TileState
emptyTileState = TileState emptyTileID 0

newtype AllTiles t = AllTiles {unAllTiles :: IOVector t}

instance Semigroup (AllTiles t) where (<>) = mappend
instance Monoid (AllTiles t) where mempty = error "Attempted to use AllTiles before it has been initialized"
instance Component (AllTiles t) where type Storage (AllTiles t) = Global (AllTiles t)

class VerdaTile t where
    emptyTile     :: t
    getTileSprite :: TileState -> t -> Sprite
    updateTileAnimation :: Word32 -> t -> t

-- As tile ID can only be created in this module, we can safely assume that this will never get given an index out of range, unless the AllTiles itself is malformed
fetchTile :: (MonadIO m, VerdaTile t) => AllTiles t -> TileID -> m t
fetchTile (AllTiles !tiles) !tID
    | tID == emptyTileID = pure emptyTile
    | otherwise          = liftIO . MVec.unsafeRead tiles . unTileID $ tID

newtype Chunks = Chunks {unChunks :: Map (V2 Int) Chunk}

instance Semigroup Chunks where (<>) = mappend
instance Monoid Chunks where mempty = Chunks Map.empty
instance Component Chunks where type Storage Chunks = Global Chunks

emptyChunks :: Chunks
emptyChunks = mempty

newtype Chunk = Chunk {unChunk :: UMVec.IOVector TileState}
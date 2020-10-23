{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.Tile where

import           Control.Lens
import           Control.Monad.IO.Class       (liftIO, MonadIO)
import           Data.Aeson
import           Data.Aeson.Types             (Parser, typeMismatch)
import           Data.Functor.Compose
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vec
import           Data.Vector.Unboxed.Deriving
import           Data.Word
import qualified System.Random                as R
import           Verda.Graphics.Sprites       (Sprite, updateFrames)
import           Verda.Parsing.Aeson
import           Verda.Parsing.Counting
import           Verda.Util.Dependency
import           Verda.Util.Flow              ((>>>=))
import           Verda.Util.Types             (Identifier, IDMap)

import           Kathu.Entity.Resource

-----------------------
-- Tile Config Types --
-----------------------

newtype TileID = TileID {unTileID :: Word32} deriving (Show, Eq, Ord)
derivingUnbox "TileID"
    [t| TileID -> Word32 |]
    [| unTileID          |]
    [| TileID            |]

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m TileID) where
    parseJSON = parseAndLookupOrAddIncrementalID TileID "TileID"

emptyTileID :: TileID
emptyTileID = TileID 0

-- | CountingIDs is initialized with this key and default map, instead of empty, so that it starts counting after reserved tile IDs
reservedTileIDMap :: (Text, Map Text Int)
reservedTileIDMap = ("TileID", Map.fromList [("empty", 0)])

newtype ToolType = ToolType Int deriving (Show, Eq, Ord)

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m ToolType) where
    parseJSON = parseAndLookupOrAddIncrementalID ToolType "ToolType"

data BreakBehavior
    = Breakable {_toolType :: ToolType, _minimumPower :: Double, _durability :: Dynamic Double}
    | Unbreakable
    deriving (Show, Eq)
makeLenses ''BreakBehavior

instance (FromJSON (Dependency s m ToolType), Monad m) => FromJSON (Dependency s m BreakBehavior) where
    parseJSON (String "unbreakable") = pure . pure $ Unbreakable
    parseJSON (Object v) = getCompose $ Breakable <$> v .:- "toolType" <*> v .:^ "minimumPower" <*> v .:^ "durability"
    parseJSON v          = typeMismatch "BreakBehavior" v

data TileRenderMode = TRMNormal Sprite | TRMRandom (Vector Sprite)

instance ( FromJSON (Dependency s m Sprite)
         , Monad m
         ) => FromJSON (Dependency s m TileRenderMode) where
    parseJSON s@(String _)   = mapDepParseJSON TRMNormal s
    parseJSON obj@(Object v) = (v .:? "mode" :: Parser (Maybe Text)) >>= \case
        Nothing       -> mapDepParseJSON TRMNormal obj
        Just "normal" -> mapDepParseJSON TRMNormal obj -- treat as a sprite if normal or no mode
        Just "random" -> composeParser (TRMRandom <$> v .:- "sprite-options")
        Just e        -> fail $ "Unknown TileRenderMode " ++ show e
    parseJSON e = typeMismatch "TileRenderMode" e

----------
-- Tile --
----------

data Tile = Tile
    { _tileID         :: !TileID
    , _tileTextID     :: !Identifier
    , _tileName       :: !Text
    , _tileRenderMode :: TileRenderMode -- lazy, emptyTiles have this undefined
    , _isSolid        :: !Bool
    , _breakBehavior  :: !BreakBehavior
    }
makeLenses ''Tile

instance ( s `CanStore` (IDMap Tile)
         , FromJSON (Dependency s m BreakBehavior)
         , FromJSON (Dependency s m Sprite)
         , FromJSON (Dependency s m TileID)
         , FromJSON (Dependency s m TileRenderMode)
         , Monad m
         ) => FromJSON (Dependency s m Tile) where
    parseJSON (Object v) = tilePar >>>= storeWithKeyFn (view tileTextID)
        where tilePar = getCompose $ Tile
                  <$> v .:- "tile-id" -- this uses the id to parse Dependency s m TileID
                  <*> v .:^ "tile-id" -- this is used for the text name
                  <*> v .:^ "name"
                  <*> v .:- "sprite"
                  <*> v .:^ "is-solid"
                  <*> v .:- "break-behavior"
    parseJSON v          = typeMismatch "Tile" v

emptyTile :: Tile
emptyTile = Tile
    { _tileID         = emptyTileID
    , _tileTextID     = "empty"
    , _tileName       = "empty tile"
    , _tileRenderMode = error "Attempted to use an empty tile's TileRenderMode"
    , _isSolid        = False
    , _breakBehavior  = Unbreakable
    }

-- Target Size: 8 bytes (for better alignment)
data TileState = TileState
    { _tile             :: {-# UNPACK #-} !TileID
    , _metadata         :: {-# UNPACK #-} !Word32
    } deriving (Show, Eq)
makeLenses ''TileState
derivingUnbox "TileState"
    [t| TileState -> (TileID, Word32) |]
    [| \(TileState tl mt) -> (tl, mt) |]
    [| uncurry TileState              |]

mkTileState :: Tile -> TileState
mkTileState t = TileState (t^.tileID) 0

emptyTileState :: TileState
emptyTileState = mkTileState emptyTile

-- | Uses randomIO to initialize metadata for tiles that make use of random properties through its metadata
mkTileStateWithMetadata :: MonadIO m => Tile -> m TileState
mkTileStateWithMetadata t
    | t^.tileID == emptyTileID = pure emptyTileState
    | otherwise                = case t^.tileRenderMode of
        TRMNormal _       -> pure $ mkTileState t
        TRMRandom sprites -> TileState (t^.tileID) . (`mod` fromIntegral (Vec.length sprites)) <$> liftIO (R.randomIO :: IO Word32)

getTileSprite :: TileState -> Tile -> Sprite
getTileSprite !tileState !tileInst = case tileInst^.tileRenderMode of
    TRMNormal sprite  -> sprite
    TRMRandom sprites -> sprites Vec.! (tileState^.metadata.to fromIntegral)

updateTileAnimation :: Word32 -> Tile -> Tile
updateTileAnimation !dT = over tileRenderMode $ \case
    TRMNormal s -> TRMNormal $ updateFrames dT s
    TRMRandom s -> TRMRandom $ updateFrames dT <$> s
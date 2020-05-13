{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.Tile where

import           Control.Lens
import           Control.Monad.IO.Class       (liftIO, MonadIO)
import           Data.Aeson
import           Data.Aeson.Types             (typeMismatch)
import           Data.Bits
import           Data.Functor.Compose
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Vector                  as Vec
import           Data.Vector.Unboxed.Deriving
import           Data.Word
import qualified System.Random                as R

import           Kathu.Entity.Resource
import           Kathu.Graphics.Drawable      (Render(..))
import           Kathu.Parsing.Aeson
import           Kathu.Parsing.Counting
import           Kathu.Util.Dependency
import           Kathu.Util.Flow              ((>>>=))
import           Kathu.Util.Types             (Identifier, IDMap)

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
    parseJSON (Object v) = getCompose $ Breakable <$> v .:~ "toolType" <*> v .:^ "minimumPower" <*> v .:^ "durability"
    parseJSON v          = typeMismatch "BreakBehavior" v

newtype TileDrawFlag = TileDrawFlag Word32 deriving (Show, Eq)

newtype TileDrawFlags = TileDrawFlags Word32 deriving (Show, Eq)

noTileDrawFlags :: TileDrawFlags
noTileDrawFlags = TileDrawFlags 0

pattern NormalMode, RandomMode :: TileDrawFlag
pattern NormalMode          = TileDrawFlag 1
pattern RandomMode          = TileDrawFlag 2  -- Instead of drawing all RenderSprites given at once, we instead draw one at random, as determined by metadata
-- pattern TiledByNeighborMode = TileDrawFlag 0b100 -- Atlas is a set of tiles which are drawn based on the surrounding tiles

isDrawFlagSet :: TileDrawFlag -> TileDrawFlags -> Bool
isDrawFlagSet _ (TileDrawFlags 0) = False
isDrawFlagSet (TileDrawFlag !flag) (TileDrawFlags !setFlags) = 0 /= flag .&. setFlags

instance FromJSON TileDrawFlag where
    parseJSON = withText "TileRenderMode" $ \case
        "normal"              -> pure NormalMode
        "choose-random-atlas" -> pure RandomMode
        m                     -> fail $ "Unknown TileRenderMode " ++ show m

instance FromJSON TileDrawFlags where
    parseJSON = withArray "TileDrawFlags" $ \arr ->
                    let nextFlag acc (TileDrawFlag f) = f .|. acc
                     in TileDrawFlags . Vec.foldl' nextFlag 0 <$> parseJSON (Array arr)

----------
-- Tile --
----------

data Tile g = Tile
    { _tileID         :: !TileID
    , _tileTextID     :: !Identifier
    , _tileName       :: !Text
    , _tileRender     :: Render g
    , _tileDrawFlags  :: !TileDrawFlags 
    , _isSolid        :: !Bool
    , _breakBehavior  :: !BreakBehavior
    }
makeLenses ''Tile

instance ( s `CanStore` (IDMap (Tile g))
         , FromJSON (Dependency s m BreakBehavior)
         , FromJSON (Dependency s m (Render g))
         , FromJSON (Dependency s m TileID)
         , Monad m
         ) => FromJSON (Dependency s m (Tile g)) where
    parseJSON (Object v) = tilePar >>>= storeWithKeyFn (view tileTextID)
        where tilePar = getCompose $ Tile
                  <$> v .:~ "tile-id" -- this uses the id to parse Dependency s m TileID
                  <*> v .:^ "tile-id" -- this is used for the text name
                  <*> v .:^ "name"
                  <*> v .:~ "render"
                  <*> v .:^? "render-flags" .!=~ noTileDrawFlags
                  <*> v .:^ "is-solid"
                  <*> v .:~ "break-behavior"
    parseJSON v          = typeMismatch "Tile" v

emptyTile :: Tile g
emptyTile = Tile
    { _tileID        = emptyTileID
    , _tileTextID    = "empty"
    , _tileName      = "empty tile"
    , _tileRender    = error "Attempted to use an empty tile's render"
    , _tileDrawFlags = noTileDrawFlags
    , _isSolid       = False
    , _breakBehavior = Unbreakable
    }

-- | Uses randomIO to initialize metadata for tiles that make use of random properties through its metadata
mkTileStateWithMetadata :: MonadIO m => Tile g -> m TileState
mkTileStateWithMetadata t = TileState (t^.tileID) . restrictRandomMeta <$> liftIO (R.randomIO :: IO Word32)
    where restrictRandomMeta = (`mod`(t^.tileRender.to (fromIntegral . Vec.length . unRender)))

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

mkTileState :: Tile g -> TileState
mkTileState t = TileState (t^.tileID) 0

emptyTileState :: TileState
emptyTileState = mkTileState emptyTile

getTileRender :: TileState -> Tile g -> Render g
getTileRender !tileState !tileInst
    | isDrawFlagSet RandomMode (tileInst^.tileDrawFlags) =
        let chooseFrame (Render layers) = Render (Vec.singleton $ layers Vec.! (tileState^.metadata.to fromIntegral))
         in chooseFrame (tileInst^.tileRender)
    | otherwise = tileInst^.tileRender
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.Tile
    ( AllTiles'
    , BreakBehavior(..)
    , Tile(..)
    , TileRenderMode(..)
    , ToolType(..)
    , makeAllTiles
    , mkTileState
    , mkTileStateWithMetadata
    , reservedTileIDMap
    , updateTileAnimation
    -- Lens
    , breakBehavior, durability, isSolid, minimumPower, tileID, tileName, tileRenderMode, tileTextID, toolType
    -- re-exported
    , TileID
    , TileState(..)
    , emptyTile
    , emptyTileID
    , emptyTileState
    ) where

import           Control.Lens
import           Control.Monad                (foldM)
import           Control.Monad.IO.Class       (liftIO, MonadIO)
import           Data.Aeson
import           Data.Aeson.Types             (Parser, typeMismatch)
import           Data.Functor                 (($>))
import           Data.Functor.Compose
import           Data.List                    (sortBy)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vec
import qualified Data.Vector.Mutable          as MVec
import           Data.Word
import qualified System.Random                as R
import           Verda.Graphics.Sprites       (Sprite, updateFrames)
import           Verda.Parsing.Aeson
import           Verda.Parsing.Counting
import           Verda.System.Tile.Components
import           Verda.Util.Dependency
import           Verda.Util.Flow              ((>>>=))
import           Verda.Util.Types             (Identifier, IDMap)

import           Kathu.Entity.Resource

-----------------------
-- Tile Config Types --
-----------------------

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

-- | CountingIDs is initialized with this key and default map, instead of empty, so that it starts counting after reserved tile IDs
reservedTileIDMap :: (Text, Map Text Int)
reservedTileIDMap = ("TileID", Map.fromList [("empty", 0)])

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

instance VerdaTile Tile where
    emptyTile = Tile
        { _tileID         = emptyTileID
        , _tileTextID     = "empty"
        , _tileName       = "empty tile"
        , _tileRenderMode = error "Attempted to use an empty tile's TileRenderMode"
        , _isSolid        = False
        , _breakBehavior  = Unbreakable
        }
    getTileSprite !tileState !tileInst = case tileInst^.tileRenderMode of
        TRMNormal sprite  -> sprite
        TRMRandom sprites -> sprites Vec.! (tileState^.tsMetadata.to fromIntegral)
    updateTileAnimation !dT = over tileRenderMode $ \case
        TRMNormal s -> TRMNormal $ updateFrames dT s
        TRMRandom s -> TRMRandom $ updateFrames dT <$> s

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

type AllTiles' = AllTiles Tile

mkTileState :: Tile -> TileState
mkTileState t = TileState (t^.tileID) 0

-- | Uses randomIO to initialize metadata for tiles that make use of random properties through its metadata
mkTileStateWithMetadata :: MonadIO m => Tile -> m TileState
mkTileStateWithMetadata t
    | t^.tileID == emptyTileID = pure emptyTileState
    | otherwise                = case t^.tileRenderMode of
        TRMNormal _       -> pure $ mkTileState t
        TRMRandom sprites -> TileState (t^.tileID) . (`mod` fromIntegral (Vec.length sprites)) <$> liftIO (R.randomIO :: IO Word64)

makeAllTiles :: Map k Tile -> IO (AllTiles Tile)
makeAllTiles elemMap = MVec.unsafeNew (Map.size elemMap) >>= \vec -> foldM (setElem vec) 0 allElems $> AllTiles vec
    where allElems = sortBy (\x y -> (x^.tileID) `compare` (y^.tileID)) . Map.elems $ elemMap
          setElem !vec !idx !e = if e^.tileID /= emptyTileID && e^.tileID.to (fromIntegral . unTileID) /= idx
                                 then error . concat $ ["Tile ", e^.tileTextID.to show, " had tile id ", e^.tileID.to show, " but was stored in index ", show idx, " in Kathu.Entity.System.makeTiles"]
                                 else MVec.unsafeWrite vec idx e $> (idx + 1)
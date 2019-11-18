{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Kathu.World.Tile where

import           Control.Lens
import           Control.Monad.IO.Class       (liftIO, MonadIO)
import           Data.Aeson
import           Data.Aeson.Types             (typeMismatch)
import           Data.Functor.Compose
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Vector                  as Vec
import           Data.Vector.Unboxed.Deriving
import           Data.Word
import qualified System.Random                as R

import           Kathu.Entity.Resource
import           Kathu.Graphics.Drawable      (Render(..), RenderSprite)
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
    [| \(TileID i) -> i  |]
    [| \(i) -> TileID i  |]

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m TileID) where
    parseJSON (String s) = pure (TileID . fromIntegral <$> lookupOrAdd "TileID" s)
    parseJSON v          = typeMismatch "TileID" v

emptyTileID :: TileID
emptyTileID = TileID 0

-- | CountingIDs is initialized with this key and default map, instead of empty, so that it starts counting after reserved tile IDs
reservedTileIDMap :: (Text, Map Text Int)
reservedTileIDMap = ("TileID", Map.fromList [("empty", 0)])

newtype ToolType = ToolType Int deriving (Show, Eq, Ord)

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m ToolType) where
    parseJSON (String s) = pure (ToolType . fromIntegral <$> lookupOrAdd "ToolType" s)
    parseJSON v          = typeMismatch "ToolType" v

data BreakBehavior
    = Breakable {_toolType :: ToolType, _minimumPower :: Double, _durability :: Dynamic Double}
    | Unbreakable
      deriving (Show, Eq)
makeLenses ''BreakBehavior

instance (FromJSON (Dependency s m ToolType), Monad m) => FromJSON (Dependency s m BreakBehavior) where
    parseJSON (String "unbreakable") = pure . pure $ Unbreakable
    parseJSON (Object v) = getCompose $ Breakable <$> v .:~ "toolType" <*> v .:^ "minimumPower" <*> v .:^ "durability"
    parseJSON v          = typeMismatch "BreakBehavior" v

----------
-- Tile --
----------

data Tile g = Tile
    { _tileID         :: TileID
    , _tileTextID     :: Identifier
    , _tileName       :: Text
    , _tileRender     :: Render g
    , _isRenderRandom :: Bool -- if True, instead of drawing all RenderSprites given at once, we instead draw one at random, as determined by metadata
    , _isSolid        :: Bool
    , _breakBehavior  :: BreakBehavior
    }
makeLenses ''Tile

instance ( s `CanStore` (IDMap (Tile g))
         , FromJSON (Dependency s m (BreakBehavior))
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
                  <*> v .:^? "should-choose-render-randomly" .!=~ False
                  <*> v .:^ "is-solid"
                  <*> v .:~ "break-behavior"
    parseJSON v          = typeMismatch "Tile" v

emptyTile :: Tile g
emptyTile = Tile
    { _tileID         = emptyTileID
    , _tileTextID     = ""
    , _tileName       = ""
    , _tileRender     = error "Attempted to draw an empty tile"
    , _isRenderRandom = False
    , _isSolid        = False
    , _breakBehavior  = Unbreakable
    }

mkTileState :: Tile g -> TileState
mkTileState t = TileState (t^.tileID) 0

-- | Uses randomIO to initialize metadata for tiles that make use of random properties through its metadata
mkTileStateWithMetadata :: MonadIO m => Tile g -> m TileState
mkTileStateWithMetadata t = (TileState (t^.tileID) . restrictRandomMeta) <$> (liftIO (R.randomIO :: IO Word32))
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
    [| \(tl, mt) -> TileState tl mt   |]

getTileRenderSprites :: TileState -> Tile g -> Vec.Vector (RenderSprite g)
getTileRenderSprites !tileState !tileInst
    | tileInst^.isRenderRandom = Vec.singleton $ (tileInst^.tileRender.to unRender) Vec.! (tileState^.metadata.to fromIntegral)
    | otherwise                = tileInst^.tileRender.to unRender
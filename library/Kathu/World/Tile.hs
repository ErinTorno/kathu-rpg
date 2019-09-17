{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MonoLocalBinds, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.World.Tile where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Lens
import Data.Functor.Compose
import Data.Text (Text)
import Data.Word
import GHC.Generics

import Kathu.Entity.Components (Render)
import Kathu.Entity.Resource
import Kathu.Parsing.Aeson
import Kathu.Parsing.Counting
import Kathu.Util.Dependency
import Kathu.Util.Flow ((>>>=))
import Kathu.Util.Types (Identifier, IDMap)

-----------------------
-- Tile Config Types --
-----------------------

newtype TileID = TileID Word16 deriving (Show, Eq)

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m TileID) where
    parseJSON (String s) = pure (TileID . fromIntegral <$> lookupOrAdd "TileID" s)
    parseJSON v          = typeMismatch "TileID" v

newtype ToolType = ToolType Int deriving (Show, Eq)

instance (s `CanStore` CountingIDs, Monad m) => FromJSON (Dependency s m ToolType) where
    parseJSON (String s) = pure (ToolType . fromIntegral <$> lookupOrAdd "ToolType" s)
    parseJSON v          = typeMismatch "ToolType" v

data BreakBehavior
    = Breakable {_toolType :: ToolType, _minimumPower :: Double, _durability :: Dynamic Double}
    | Unbreakable
      deriving (Show, Eq, Generic)
makeLenses ''BreakBehavior

instance (FromJSON (Dependency s m ToolType), Monad m) => FromJSON (Dependency s m BreakBehavior) where
    parseJSON (String "unbreakable") = pure . pure $ Unbreakable
    parseJSON (Object v) = getCompose $ Breakable <$> v .:~ "toolType" <*> v .:^ "minimumPower" <*> v .:^ "durability"
    parseJSON v          = typeMismatch "BreakBehavior" v

----------
-- Tile --
----------

data Tile g = Tile
    { _tileID        :: TileID
    , _tileTextID    :: Identifier
    , _tileName      :: Text
    , _tileRender    :: Render g
    , _isSolid       :: Bool
    , _breakBehavior :: BreakBehavior
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
                  <*> v .:^ "is-solid"
                  <*> v .:~ "break-behavior"
    parseJSON v          = typeMismatch "Tile" v

emptyTileID :: TileID
emptyTileID = TileID 0

emptyTile :: Tile g
emptyTile = Tile
    { _tileID = emptyTileID
    , _tileTextID = ""
    , _tileName = ""
    , _tileRender = error "Attempted to draw an empty tile"
    , _isSolid = False
    , _breakBehavior = Unbreakable
    }

mkTileState :: Tile g -> TileState g
mkTileState t = TileState t 0

data TileState g = TileState
    { _tile             :: !(Tile g)
    , _metadata         :: !Word8
    }
makeLenses ''TileState
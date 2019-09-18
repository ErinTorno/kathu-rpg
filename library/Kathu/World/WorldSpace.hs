{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Kathu.World.WorldSpace where

import Apecs hiding (Map)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Linear.V3 (V3(..))

import Kathu.Entity.Components
import Kathu.Entity.Item
import Kathu.Entity.Prototype
import Kathu.Graphics.Color (black)
import Kathu.Graphics.Drawable (Render)
import Kathu.Graphics.Palette
import Kathu.Util.Apecs
import Kathu.Util.Dependency
import Kathu.Util.Flow ((>>>=))
import Kathu.Util.MultiDimVector (fromList3D)
import Kathu.Util.Types (Identifier, IDMap)
import Kathu.World.Field
import Kathu.World.Tile (Tile)

data WorldSpace g = WorldSpace
    { worldID       :: Identifier
    , worldName     :: Text
    , worldPalettes :: Vector Palette
    , loadPoint     :: V3 Float
    , worldEntities :: Vector (V3 Float, EntityPrototype g)
    , worldItems    :: Vector (V3 Float, ItemStack g)
    , worldFields   :: FieldSet
    -- Some way to hold the fields that it possesses
    }

emptyWorldSpace :: WorldSpace g
emptyWorldSpace = WorldSpace "" "the void" (Vec.singleton (Palette black Nothing)) (V3 0 0 0) Vec.empty Vec.empty Map.empty

-- right now we only consider horizontal fields; ones with different z depths are ignored
fieldsSurrounding :: RealFrac a => V3 a -> WorldSpace g -> [(V3 Int, Field)]
fieldsSurrounding v ws = catMaybes $ readFields [] (ox - 1) (oy - 1)
    where fields    = worldFields ws
          (V3 ox oy oz) = fieldContainingCoord v
          readFields !acc !x !y | y > oy + 1 = acc
                                | x > ox + 1 = readFields acc 0 (y + 1)
                                | otherwise  = readFields (((curV,) <$> Map.lookup curV fields):acc) (x + 1) y
              where curV = V3 x y oz -- only consider same z level right now

--------------------
-- System Related --
--------------------

initWorldSpace :: forall w m g. (MonadIO m, Get w m EntityCounter, HasEach w m '[Local, Position, Render g, WorldSpace g])
               => (EntityPrototype g -> SystemT w m Entity) -> WorldSpace g -> SystemT w m ()
initWorldSpace mkEntity ws = do
    global $= ws
    cmap $ \(Local _)   -> (Position . loadPoint $ ws)
    -- we place all items in the world as entities
    mapM_ (\(pos, ety)  -> mkEntity ety >>= (flip set) (Position pos)) (worldEntities ws)
    mapM_ (\(pos, item) -> newEntityFromItem item pos) (worldItems ws)

newEntityFromItem :: forall w m g. (MonadIO m, Get w m EntityCounter, HasEach w m '[Position, Render g]) => ItemStack g -> V3 Float -> SystemT w m Entity
newEntityFromItem stack v = newEntity (Position v, itemIcon . stackItem $ stack)
          -- as of right now, count not considered; this will be added when picking up is implemented

-------------------
-- Serialization --
-------------------

-- Instances and functions related to serializing the WorldSpace
-- This is kept at the end due to the length and complexity of theses

instance ( s `CanProvide` (IDMap (EntityPrototype g))
         , s `CanProvide` (IDMap (Tile g))
         , FromJSON (Dependency s m (ItemStack g))
         , MonadIO m
         ) => FromJSON (Dependency s m (WorldSpace g)) where
    parseJSON (Object v) = do
        worldId  <- v .: "world-id"
        wName    <- v .: "name"
        palettes <- v .: "palettes"
        loadPnt  <- (*unitsPerTile) <$> v .: "load-point"
        layers :: [[[Char]]] <- v .: "layers"
        legend <- do
            (keys, vals) :: ([Text], [Identifier]) <- ((unzip . Map.toList) <$> v .: "legend")
            let dLookup = dependencyMapLookupElseError :: String -> Identifier -> Dependency s m (Tile g)
             in pure . fmap (Map.fromList . zip (T.head <$> keys)) . flattenDependency . fmap (dLookup "Tile") $ vals
        let parsePlacement fn (Array vec) = foldM (parseIndivPlace fn) (pure []) vec
            parsePlacement _ e            = typeMismatch "Placement" e
            parseIndivPlace fn acc val@(Object obj) = do
                pos   <- (*unitsPerTile) <$> obj .: "position"
                stack <- fn val
                pure $ acc >>= \ls -> ((:ls) . (pos,)) <$> stack
            parseIndivPlace _ _ e                 = typeMismatch "Placement" e
            parseEty = withObject "EntityPlacement" $ \obj -> (obj .: "entity" :: Parser Identifier) >>= pure . dependencyMapLookupElseError "Entity" 
        items    <- v .: "items" >>= parsePlacement parseJSON >>>= pure . Vec.fromList
        entities <- v .: "entities" >>= parsePlacement parseEty >>>= pure . Vec.fromList
        let layersVec     = (\lgnd -> fromList3D . fmap (fmap (fmap (fromMaybe failIfNothing . (flip Map.lookup) lgnd))) $ layers) <$> legend
            failIfNothing = error "Attempted to tile without a listing in the WorldSpace's legend"
        pure $ WorldSpace worldId wName palettes loadPnt <$> entities <*> items <*> (liftDependency . fromTileVector3D =<< layersVec)
    parseJSON e          = typeMismatch "WorldSpace" e
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.App.Data.KathuStore where

import Control.Lens
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified SDL

import Kathu.App.Graphics.Image (Image, ImageID(..))
import Kathu.Entity.Damage (DamageProfile)
import Kathu.Entity.Item (Item)
import Kathu.Entity.Prototype
import Kathu.Graphics.Drawable
import Kathu.IO.Directory (WorkingDirectory)
import Kathu.Parsing.Counting (CountingIDs(..))
import Kathu.Util.Dependency
import Kathu.Util.Types (IDMap)
import Kathu.World.Tile (Tile, emptyTile)

data KathuStore = KathuStore
    { _plImages         :: Vector Image
    , _countingIDs      :: CountingIDs
    , _plEntities       :: IDMap (EntityPrototype ImageID)
    , _plItems          :: IDMap (Item ImageID)
    , _plTiles          :: IDMap (Tile ImageID)
    , _damageProfiles   :: IDMap (DamageProfile (RenderSprite ImageID))
    , _workingDirectory :: WorkingDirectory
    }
makeLenses ''KathuStore

emptyKathuStore :: KathuStore
emptyKathuStore = KathuStore
    { _plImages = Vec.empty
    -- we automatically place the empty tile at the start of this, just so index 0 is preserved for it
    , _countingIDs = CountingIDs . Map.fromList $ [("TileID", Map.fromList [("empty", 0)])]
    , _plEntities = Map.empty
    , _plItems = Map.empty
    , _plTiles = Map.fromList [("empty", emptyTile)]
    , _damageProfiles = Map.empty
    , _workingDirectory = ""
    }

--------------------------
-- Dependency instances --
--------------------------

instance KathuStore `CanStore` WorkingDirectory where storeLens = workingDirectory
instance KathuStore `CanProvide` WorkingDirectory

instance KathuStore `CanStore` CountingIDs where storeLens = countingIDs
instance KathuStore `CanProvide` CountingIDs

instance KathuStore `CanStore` (IDMap (DamageProfile (RenderSprite ImageID))) where storeLens = damageProfiles
instance KathuStore `CanProvide` (IDMap (DamageProfile (RenderSprite ImageID)))

instance KathuStore `CanStore` (IDMap (EntityPrototype ImageID)) where storeLens = plEntities
instance KathuStore `CanProvide` (IDMap (EntityPrototype ImageID))

instance KathuStore `CanStore` (IDMap (Item ImageID)) where storeLens = plItems
instance KathuStore `CanProvide` (IDMap (Item ImageID))

instance KathuStore `CanStore` (IDMap (Tile ImageID)) where storeLens = plTiles
instance KathuStore `CanProvide` (IDMap (Tile ImageID))

instance KathuStore `CanStore` (Vector Image) where storeLens = plImages
instance KathuStore `CanProvide` (Vector Image)

instance KathuStore `CanProvide` (ImageBounds (Dependency KathuStore IO) ImageID) where
    provide = (ImageBounds . getFn) <$> provide
        where getFn images = \(ImageID iid) -> (liftDependency . SDL.surfaceDimensions . (Vec.!) images $ iid)
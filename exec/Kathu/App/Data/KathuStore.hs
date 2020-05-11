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
import Kathu.Entity.Physics.Floor (FloorProperty, reservedFloorIDMap)
import Kathu.Entity.Prototype
import Kathu.Graphics.Drawable
import Kathu.Graphics.Palette
import Kathu.IO.Directory (WorkingDirectory)
import Kathu.Parsing.Counting (CountingIDs(..))
import Kathu.Util.Dependency
import Kathu.Util.Types (IDMap)
import Kathu.World.Tile (emptyTile, reservedTileIDMap, Tile)

data KathuStore = KathuStore
    { _plImages         :: !(Vector Image)
    , _countingIDs      :: !CountingIDs
    , _plEntities       :: !(IDMap (EntityPrototype ImageID))
    , _plFloors         :: !(IDMap FloorProperty)
    , _plItems          :: !(IDMap (Item ImageID))
    , _plTiles          :: !(IDMap (Tile ImageID))
    , _plPalettes       :: !(IDMap Palette)
    , _damageProfiles   :: !(IDMap (DamageProfile (RenderSprite ImageID)))
    , _workingDirectory :: !WorkingDirectory
    }
makeLenses ''KathuStore

emptyKathuStore :: KathuStore
emptyKathuStore = KathuStore
    { _plImages = Vec.empty
    -- we automatically certain reserved values in this counting map, so that we start counting after them
    , _countingIDs = CountingIDs . Map.fromList $ [reservedTileIDMap, reservedFloorIDMap]
    , _plEntities = Map.empty
    , _plFloors = Map.empty
    , _plItems = Map.empty
    , _plTiles = Map.fromList [("empty", emptyTile)]
    , _plPalettes = Map.empty
    , _damageProfiles = Map.empty
    , _workingDirectory = ""
    }

--------------------------
-- Dependency instances --
--------------------------

instance KathuStore `CanStore`   WorkingDirectory where storeLens = workingDirectory
instance KathuStore `CanProvide` WorkingDirectory

instance KathuStore `CanStore`   CountingIDs where storeLens = countingIDs
instance KathuStore `CanProvide` CountingIDs

instance KathuStore `CanStore`   IDMap (DamageProfile (RenderSprite ImageID)) where storeLens = damageProfiles
instance KathuStore `CanProvide` IDMap (DamageProfile (RenderSprite ImageID))

instance KathuStore `CanStore`   IDMap (EntityPrototype ImageID) where storeLens = plEntities
instance KathuStore `CanProvide` IDMap (EntityPrototype ImageID)

instance KathuStore `CanStore`   IDMap FloorProperty where storeLens = plFloors
instance KathuStore `CanProvide` IDMap FloorProperty

instance KathuStore `CanStore`   IDMap (Item ImageID) where storeLens = plItems
instance KathuStore `CanProvide` IDMap (Item ImageID)

instance KathuStore `CanStore`   IDMap (Tile ImageID) where storeLens = plTiles
instance KathuStore `CanProvide` IDMap (Tile ImageID)

instance KathuStore `CanStore`   IDMap Palette where storeLens = plPalettes
instance KathuStore `CanProvide` IDMap Palette

instance KathuStore `CanStore`   Vector Image where storeLens = plImages
instance KathuStore `CanProvide` Vector Image

instance KathuStore `CanProvide` (ImageBounds (Dependency KathuStore IO) ImageID) where
    provide = ImageBounds . getFn <$> provide
        where getFn images (ImageID iid) = liftDependency . SDL.surfaceDimensions . (Vec.!) images $ iid
{-# LANGUAGE TemplateHaskell #-}

module Kathu.App.Data.KathuStore where

import           Control.Lens
import qualified Data.Map as Map
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified SDL
import           Verda.IO.Directory (WorkingDirectory)
import           Verda.Parsing.Counting (CountingIDs(..))
import           Verda.Util.Dependency
import           Verda.Util.Types (IDMap, emptyIDMap)

import           Kathu.App.Graphics.Image (Image, ImageID(..))
import           Kathu.Entity.Item (Item)
import           Kathu.Entity.Physics.Floor (FloorProperty, reservedFloorIDMap)
import           Kathu.Entity.Prototype
import           Kathu.Graphics.Drawable
import           Kathu.Graphics.Palette
import           Kathu.World.Tile (emptyTile, reservedTileIDMap, Tile)

data KathuStore = KathuStore
    { _psCountingIDs      :: !CountingIDs
    , _psEntities         :: !(IDMap (EntityPrototype ImageID))
    , _psImages           :: !(Vector Image)
    , _psItems            :: !(IDMap (Item ImageID))
    , _psFloors           :: !(IDMap FloorProperty)
    , _psPalettes         :: !(IDMap Palette)
    , _psTiles            :: !(IDMap (Tile ImageID))
    , _psWorkingDirectory :: !WorkingDirectory
    }
makeLenses ''KathuStore

emptyKathuStore :: KathuStore
emptyKathuStore = KathuStore
    -- we automatically certain reserved values in this counting map, so that we start counting after them
    { _psCountingIDs      = CountingIDs . Map.fromList $ [reservedTileIDMap, reservedFloorIDMap]
    , _psEntities         = emptyIDMap
    , _psImages           = Vec.empty
    , _psItems            = emptyIDMap
    , _psFloors           = emptyIDMap
    , _psPalettes         = emptyIDMap
    , _psTiles            = Map.singleton "empty" emptyTile
    , _psWorkingDirectory = ""
    }

--------------------------
-- Dependency instances --
--------------------------

instance KathuStore `CanStore`   WorkingDirectory where storeLens = psWorkingDirectory
instance KathuStore `CanProvide` WorkingDirectory

instance KathuStore `CanStore`   CountingIDs where storeLens = psCountingIDs
instance KathuStore `CanProvide` CountingIDs

instance KathuStore `CanStore`   IDMap (EntityPrototype ImageID) where storeLens = psEntities
instance KathuStore `CanProvide` IDMap (EntityPrototype ImageID)

instance KathuStore `CanStore`   IDMap FloorProperty where storeLens = psFloors
instance KathuStore `CanProvide` IDMap FloorProperty

instance KathuStore `CanStore`   IDMap (Item ImageID) where storeLens = psItems
instance KathuStore `CanProvide` IDMap (Item ImageID)

instance KathuStore `CanStore`   IDMap (Tile ImageID) where storeLens = psTiles
instance KathuStore `CanProvide` IDMap (Tile ImageID)

instance KathuStore `CanStore`   IDMap Palette where storeLens = psPalettes
instance KathuStore `CanProvide` IDMap Palette

instance KathuStore `CanStore`   Vector Image where storeLens = psImages
instance KathuStore `CanProvide` Vector Image

instance KathuStore `CanProvide` (ImageBounds (Dependency KathuStore IO) ImageID) where
    provide = ImageBounds . getFn <$> provide
        where getFn images (ImageID iid) = liftDependency . SDL.surfaceDimensions . (Vec.!) images $ iid
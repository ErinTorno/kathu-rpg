{-# LANGUAGE TemplateHaskell #-}

module Kathu.App.Data.KathuStore where

import           Control.Lens
import qualified Data.Map                   as Map
import qualified Data.Vector                as Vec
import qualified SDL
import           Verda.Graphics.Fonts       (Font)
import           Verda.Graphics.Sprites     (SpriteID(..), SurfaceVector)
import           Verda.IO.Directory         (WorkingDirectory)
import           Verda.Parsing.Counting     (CountingIDs(..))
import           Verda.Util.Dependency
import           Verda.Util.Types           (IDMap, emptyIDMap)

import           Kathu.Entity.Item          (Item)
import           Kathu.Entity.Physics.Floor (FloorProperty, reservedFloorIDMap)
import           Kathu.Entity.Prefab
import           Kathu.Graphics.Drawable
import           Kathu.Graphics.Palette
import           Kathu.World.Tile           (emptyTile, reservedTileIDMap, Tile)

data KathuStore = KathuStore
    { _psCountingIDs      :: !CountingIDs
    , _psItems            :: !(IDMap (Item SpriteID))
    , _psFloors           :: !(IDMap FloorProperty)
    , _psFonts            :: !(IDMap Font)
    , _psPalettes         :: !(IDMap Palette)
    , _psPrefabs          :: !(IDMap Prefab)
    , _psSurfaces         :: !SurfaceVector
    , _psTiles            :: !(IDMap (Tile SpriteID))
    , _psWorkingDirectory :: !WorkingDirectory
    }
makeLenses ''KathuStore

emptyKathuStore :: KathuStore
emptyKathuStore = KathuStore
    -- we automatically certain reserved values in this counting map, so that we start counting after them
    { _psCountingIDs      = CountingIDs . Map.fromList $ [reservedTileIDMap, reservedFloorIDMap]
    , _psItems            = emptyIDMap
    , _psFloors           = emptyIDMap
    , _psFonts            = emptyIDMap
    , _psPalettes         = emptyIDMap
    , _psPrefabs          = emptyIDMap
    , _psSurfaces         = Vec.empty
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

instance KathuStore `CanStore`   IDMap FloorProperty where storeLens = psFloors
instance KathuStore `CanProvide` IDMap FloorProperty

instance KathuStore `CanStore`   IDMap Font where storeLens = psFonts
instance KathuStore `CanProvide` IDMap Font

instance KathuStore `CanStore`   IDMap (Item SpriteID) where storeLens = psItems
instance KathuStore `CanProvide` IDMap (Item SpriteID)

instance KathuStore `CanStore`   IDMap Prefab where storeLens = psPrefabs
instance KathuStore `CanProvide` IDMap Prefab

instance KathuStore `CanStore`   IDMap (Tile SpriteID) where storeLens = psTiles
instance KathuStore `CanProvide` IDMap (Tile SpriteID)

instance KathuStore `CanStore`   IDMap Palette where storeLens = psPalettes
instance KathuStore `CanProvide` IDMap Palette

instance KathuStore `CanStore`   SurfaceVector where storeLens = psSurfaces
instance KathuStore `CanProvide` SurfaceVector

instance KathuStore `CanProvide` (ImageBounds (Dependency KathuStore IO) SpriteID) where
    provide = ImageBounds . getFn <$> provide
        where getFn images (SpriteID iid) = liftDependency . SDL.surfaceDimensions . (Vec.!) images $ iid
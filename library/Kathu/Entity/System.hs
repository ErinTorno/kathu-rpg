{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we need orphan instances to set up the Apecs system

module Kathu.Entity.System where

import           Apecs hiding (Map)
import           Control.Monad (foldM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Lens
import           Data.List (sortBy)
import           Data.Functor (($>))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Mutable as MVec
import           Data.Vector.Mutable (IOVector)
import           Data.Word
import qualified System.Random as R
import           Verda.Time
import           Verda.Util.Types (IDMap)

import           Kathu.Entity.Physics.Floor (FloorPropEntity)
import           Kathu.Graphics.Palette (PaletteManager, staticManager)
import           Kathu.Scripting.Variables (Variables)
import           Kathu.World.Stasis
import           Kathu.World.Tile (Tile(..), TileID(..), tileID, TileState(..), tileTextID, unTileID)
import           Kathu.World.Time (WorldTime(..))

-- New Globals

instance Semigroup WorldTime where (<>) = mappend
instance Monoid WorldTime where mempty  = WorldTime 0
instance Component WorldTime where type Storage WorldTime = Global WorldTime

instance Semigroup PaletteManager where (<>) = mappend
instance Monoid PaletteManager where mempty  = staticManager 0
instance Component PaletteManager where type Storage PaletteManager = Global PaletteManager

newtype  Random = Random R.StdGen
instance Semigroup Random where (<>) = mappend
instance Monoid Random where mempty  = Random $ R.mkStdGen 0 -- the IO portion of this is expected to initialize it with a seed
instance Component Random where type Storage Random = Global Random

newtype  Tiles = Tiles (IOVector Tile)
instance Semigroup Tiles where (<>) = mappend
instance Monoid Tiles where mempty  = error "Attempted to use Tiles before it has been loaded"
instance Component Tiles where type Storage Tiles = Global Tiles

instance Semigroup WorldStases where (<>) = mappend
instance Monoid WorldStases where mempty = WorldStases Map.empty
instance Component WorldStases where type Storage WorldStases = Global WorldStases

data FloorProperties = FloorProperties {propsDefault :: FloorPropEntity, propsAll :: IDMap FloorPropEntity}
instance Semigroup FloorProperties where (<>) = mappend
instance Monoid FloorProperties where mempty  = error "Attempted to access the FloorProperties before it has been initialized"
instance Component FloorProperties where type Storage FloorProperties = Global FloorProperties

instance Semigroup Variables where (<>) = mappend
instance Monoid Variables where mempty = error "Attempted to access Variables global component before it has been initialized"
instance Component Variables where type Storage Variables = Global Variables

---------------------------
-- Debug-related globals --
---------------------------

-- Similar to Debug, but not meant to be toggled frequently
-- | Additional editor information should be added to entities when this is True
newtype  IncludeEditorInfo = IncludeEditorInfo {unEditorInfo :: Bool}
instance Semigroup IncludeEditorInfo where (<>) = mappend
instance Monoid IncludeEditorInfo where mempty  = IncludeEditorInfo False
instance Component IncludeEditorInfo where type Storage IncludeEditorInfo = Global IncludeEditorInfo

-- Entity functions

stepLogicTime :: forall w m. (Has w m LogicTime, MonadIO m) => Word32 -> SystemT w m ()
stepLogicTime !dT = modify global $ \(LogicTime t) -> LogicTime (t + dT)

stepRenderTime :: forall w m. (Has w m RenderTime, MonadIO m) => Word32 -> SystemT w m ()
stepRenderTime !dT = modify global $ \(RenderTime t) -> RenderTime (t + dT)

stepWorldTime :: forall w m. (Has w m WorldTime, MonadIO m) => Word32 -> SystemT w m ()
stepWorldTime !dT = modify global $ \(WorldTime t) -> WorldTime (t + fromIntegral dT)

-------------
-- Unsafe! --
-------------

fromTiles :: Tiles -> TileState -> IO Tile
fromTiles tiles (TileState tid _) = fromTilesID tiles tid
-------------

fromTilesID :: Tiles -> TileID -> IO Tile
fromTilesID (Tiles vec) (TileID tid) = MVec.read vec . fromIntegral $ tid

makeTiles :: Map k Tile -> IO Tiles
makeTiles elemMap = MVec.unsafeNew (Map.size elemMap) >>= \vec -> foldM (setElem vec) 0 allElems $> Tiles vec
    where allElems = sortBy (\x y -> (x^.tileID) `compare` (y^.tileID)) . Map.elems $ elemMap
          setElem !vec !idx !e = if e^.tileID.to (fromIntegral . unTileID) /= idx
                                 then error . concat $ ["Tile ", e^.tileTextID.to show, " had tile id ", e^.tileID.to show, " but was stored in index ", show idx, " in Kathu.Entity.System.makeTiles"]
                                 else MVec.unsafeWrite vec idx e $> (idx + 1)
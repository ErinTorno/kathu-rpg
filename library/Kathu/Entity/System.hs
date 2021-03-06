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

import           Kathu.Entity.Physics.Floor (FloorPropEntity)
import           Kathu.Entity.Logger
import           Kathu.Entity.Time
import           Kathu.Graphics.Palette (PaletteManager, staticManager)
import           Kathu.Scripting.Variables (Variables)
import           Kathu.Util.Types (IDMap)
import           Kathu.World.Stasis
import           Kathu.World.Tile (Tile(..), TileID(..), tileID, TileState(..), tileTextID, unTileID)
import           Kathu.World.Time (WorldTime(..))

-- New Globals

instance Semigroup LogicTime where (<>) = mappend
instance Monoid LogicTime where mempty  = LogicTime 0
instance Component LogicTime where type Storage LogicTime = Global LogicTime

instance Semigroup RenderTime where (<>) = mappend
instance Monoid RenderTime where mempty  = RenderTime 0
instance Component RenderTime where type Storage RenderTime = Global RenderTime

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

newtype  Tiles g = Tiles (IOVector (Tile g))

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

-- | A general debug flag that prompts for additional game information to be visible by the user
newtype  Debug = Debug {unDebug :: Bool}
instance Semigroup Debug where (<>) = mappend
instance Monoid Debug where mempty  = Debug False
instance Component Debug where type Storage Debug = Global Debug

-- Similar to Debug, but not meant to be toggled frequently
-- | Additional editor information should be added to entities when this is True
newtype  IncludeEditorInfo = IncludeEditorInfo {unEditorInfo :: Bool}
instance Semigroup IncludeEditorInfo where (<>) = mappend
instance Monoid IncludeEditorInfo where mempty  = IncludeEditorInfo False
instance Component IncludeEditorInfo where type Storage IncludeEditorInfo = Global IncludeEditorInfo

instance Semigroup Logger where (<>) = mappend
instance Monoid Logger where mempty = defaultLogger
instance Component Logger where type Storage Logger = Global Logger

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

fromTiles :: Tiles g -> TileState -> IO (Tile g)
fromTiles tiles (TileState tid _) = fromTilesID tiles tid
-------------

fromTilesID :: Tiles g -> TileID -> IO (Tile g)
fromTilesID (Tiles vec) (TileID tid) = MVec.read vec . fromIntegral $ tid

makeTiles :: Map k (Tile g) -> IO (Tiles g)
makeTiles elemMap = MVec.unsafeNew (Map.size elemMap) >>= \vec -> foldM (setElem vec) 0 allElems $> Tiles vec
    where allElems = sortBy (\x y -> (x^.tileID) `compare` (y^.tileID)) . Map.elems $ elemMap
          setElem !vec !idx !e = if e^.tileID.to (fromIntegral . unTileID) /= idx
                                 then error . concat $ ["Tile ", e^.tileTextID.to show, " had tile id ", e^.tileID.to show, " but was stored in index ", show idx, " in Kathu.Entity.System.makeTiles"]
                                 else MVec.unsafeWrite vec idx e $> (idx + 1)
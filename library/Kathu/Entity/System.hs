{-# OPTIONS_GHC -fno-warn-orphans #-}
-- we need orphan instances to set up the Apecs system

module Kathu.Entity.System where

import           Apecs hiding (Map)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as Map
import           Data.Word
import qualified System.Random as R
import           Verda.Util.Types (IDMap)

import           Kathu.Entity.Physics.Floor (FloorPropEntity)
import           Kathu.Graphics.Palette (PaletteManager, staticManager)
import           Kathu.Scripting.Variables (Variables)
import           Kathu.World.Stasis
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

stepWorldTime :: forall w m. (Has w m WorldTime, MonadIO m) => Word32 -> SystemT w m ()
stepWorldTime !dT = modify global $ \(WorldTime t) -> WorldTime (t + fromIntegral dT)

-------------
-- Unsafe! --
-------------
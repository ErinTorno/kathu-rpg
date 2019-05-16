{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

-- Required for Apecs
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeFamilies #-}

module Kathu.Entity.System where

import Apecs hiding (Map)
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Word
import Kathu.Entity.Components
import Kathu.Entity.Item
import Kathu.Entity.Prototype
import Kathu.Graphics.Drawable (Image)
import Kathu.Graphics.ImageManager
import Kathu.Graphics.UI
import Kathu.IO.Settings
import Kathu.World.Tile (Tile)
import Kathu.World.WorldSpace (WorldSpace, emptyWorldSpace)
import qualified System.Random as R


-- We create an entity prototype that supports all given component types
defineData          "EntityPrototype" "" allSerialComponents
defineEntityCreator "newFromPrototype" "" allSerialComponents

-- Globals

-- these are kept separate, as the times for each type of loop may become desynced
-- ex: We may update the physics multiple times to "catch-up" if there was a delay in the system
--     Or update the graphics multiple times to display higher frames while we wait for the physics to run again
newtype LogicTime = LogicTime (Word32) deriving (Show, Eq)
instance Semigroup LogicTime where (<>) = mappend
instance Monoid LogicTime where mempty = LogicTime 0
instance Component LogicTime where type Storage LogicTime = Global LogicTime

newtype RenderTime = RenderTime (Word32) deriving (Show, Eq)
instance Semigroup RenderTime where (<>) = mappend
instance Monoid RenderTime where mempty = RenderTime 0
instance Component RenderTime where type Storage RenderTime = Global RenderTime

newtype Random = Random (R.StdGen)
instance Semigroup Random where (<>) = mappend
instance Monoid Random where mempty = Random $ R.mkStdGen 0 -- the IO portion of this is expected to initialize it with a seed
instance Component Random where type Storage Random = Global Random

instance Semigroup Settings where (<>) = mappend
instance Monoid Settings where mempty = defaultSettings
instance Component Settings where type Storage Settings = Global Settings

instance Semigroup ImageManager where (<>) = mappend
instance Monoid ImageManager where mempty = defaultImageManager
instance Component ImageManager where type Storage ImageManager = Global ImageManager

instance Semigroup UIConfig where (<>) = mappend
instance Monoid UIConfig where mempty = error "Attempted to use UIConfig before it has been loaded"
instance Component UIConfig where type Storage UIConfig = Global UIConfig

instance Semigroup WorldSpace where (<>) = mappend
instance Monoid WorldSpace where mempty = emptyWorldSpace
instance Component WorldSpace where type Storage WorldSpace = Global WorldSpace

newtype Debug = Debug Bool
instance Semigroup Debug where (<>) = mappend
instance Monoid Debug where mempty = Debug False
instance Component Debug where type Storage Debug = Global Debug

-- | This data type plays the role as a collection of named values for the game to read from when loading a level
data Library = Library
    { _images      :: Vector Image
    , _uiConfig    :: UIConfig
    , _prototypes  :: Map Text EntityPrototype
    , _items       :: Map Text Item
    , _tiles       :: Map Text Tile
    , _worldSpaces :: Map Text WorldSpace
    }
makeLenses ''Library

instance Semigroup Library where (<>) = mappend
instance Monoid Library where mempty = Library Vec.empty mempty Map.empty Map.empty Map.empty Map.empty
instance Component Library where type Storage Library = Global Library

-- World

makeWorld "EntityWorld" $ allNonGlobal ++ [''LogicTime, ''RenderTime, ''Random, ''Settings, ''ImageManager, ''UIConfig, ''WorldSpace, ''Library, ''Debug]

type System' a = System EntityWorld a
type SystemT' m a = SystemT EntityWorld m a

-- Entity functions

destroyEntity ety = destroy ety (Proxy @AllComponents)

stepLogicTime :: Word32 -> System' ()
stepLogicTime !dT = modify global $ \(LogicTime t) -> LogicTime (t + dT)

stepRenderTime :: Word32 -> System' ()
stepRenderTime !dT = modify global $ \(RenderTime t) -> RenderTime (t + dT)
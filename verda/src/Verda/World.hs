{-# LANGUAGE TemplateHaskell #-}

module Verda.World
    ( DeletableBaseVerdaComponents
    , Existance(..)
    , FollowParent(..)
    , IsQuitting(..)
    , VerdaWorld
    , baseVerdaComponentNames
    -- re-exported
    , Position(..)
    ) where

import           Apecs
import           Apecs.Physics                (Position(..))
import           Language.Haskell.TH.Syntax   (Name)

import           Verda.Event.Controls
import           Verda.Graphics.Components
import           Verda.Logger
import           Verda.Time
import           Verda.Util.Apecs

-- TODO find a way to move this into Kathu
type CacheSize = 4096

----------------
-- Components --
----------------

-- | A component that we enforce all created entities can hold without any differences
data Existance = Existance deriving (Show, Eq)
instance Component Existance where type Storage Existance = Cache CacheSize (Map Existance)

-- | Ensure the Entity's Position will always match the position of the parent Entity
newtype FollowParent = FollowParent {unFollowParent :: Entity} deriving (Show, Eq)
instance Component FollowParent where type Storage FollowParent = Cache CacheSize (Map FollowParent)

------------
-- Global --
------------

newtype IsQuitting = IsQuitting Bool
instance Semigroup IsQuitting where (<>) = mappend
instance Monoid IsQuitting where mempty = IsQuitting False
instance Component IsQuitting where type Storage IsQuitting = Global IsQuitting

----------
-- Misc --
----------
-- Position is treated differently in the following due to its inclusion in Physics

-- | For use as a constraint to ensure all components required by Verda are available
type VerdaWorld w m = ReadWriteEach w m
   '[ BackgroundColor, ControlState, CursorMotionState, FontCache, IsQuitting, Logger, LogicTime, RenderTime, Resolution, SpriteManager
    , Camera
    , Existance, FollowParent, Position, Sprite, Tint
    ]

baseVerdaComponentNames :: [Name]
baseVerdaComponentNames =
    [ ''BackgroundColor, ''ControlState, ''CursorMotionState, ''FontCache, ''IsQuitting, ''Logger, ''LogicTime, ''RenderTime, ''Resolution, ''SpriteManager
    , ''Camera
    , ''Existance, ''FollowParent, ''Sprite, ''Tint
    ]

type DeletableBaseVerdaComponents =
    ( Existance
    , FollowParent
    , (Sprite, Tint)
    )
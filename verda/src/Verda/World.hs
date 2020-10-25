{-# LANGUAGE TemplateHaskell #-}

module Verda.World
    ( DeletableBaseVerdaComponents
    , Existance(..)
    , FollowParent(..)
    , IsDebug(..)
    , IsQuitting(..)
    , VerdaWorld
    , addRendererExtension
    , addSpriteRenderExtension
    , baseVerdaComponentNames
    , initVerdaWorld
    -- re-exported
    , Position(..)
    ) where

import           Apecs
import           Apecs.Physics                (Position(..))
import           Control.Monad.IO.Class       (MonadIO)
import qualified Data.Vector                  as Vec
import           Language.Haskell.TH.Syntax   (Name)
import           Linear.V2
import qualified SDL

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

-- | A general debug flag that prompts for additional game information to be visible by the user
newtype IsDebug = IsDebug {unDebug :: Bool}
instance Semigroup IsDebug where (<>) = mappend
instance Monoid IsDebug where mempty = IsDebug False
instance Component IsDebug where type Storage IsDebug = Global IsDebug

newtype IsQuitting = IsQuitting {unQuitting :: Bool}
instance Semigroup IsQuitting where (<>) = mappend
instance Monoid IsQuitting where mempty = IsQuitting False
instance Component IsQuitting where type Storage IsQuitting = Global IsQuitting

----------
-- Misc --
----------
-- Position is treated differently in the following due to its inclusion in Physics

-- | For use as a constraint to ensure all components required by Verda are available
type VerdaWorld w m = ReadWriteEach w m
   '[ BackgroundColor, ControlState, CursorMotionState, FontCache, IsDebug, IsQuitting, Logger, LogicTime, RenderExtensions, RenderTime, Resolution, SpriteManager
    , Camera
    , Existance, FollowParent, Position, Sprite, Tint
    ]

baseVerdaComponentNames :: [Name]
baseVerdaComponentNames =
    [ ''BackgroundColor, ''ControlState, ''CursorMotionState, ''FontCache, ''IsDebug, ''IsQuitting, ''Logger, ''LogicTime, ''RenderExtensions, ''RenderTime, ''Resolution, ''SpriteManager
    , ''Camera
    , ''Existance, ''FollowParent, ''Sprite, ''Tint
    ]

type DeletableBaseVerdaComponents =
    ( Existance
    , FollowParent
    , (Sprite, Tint)
    )

------------------
-- Utils / Init --
------------------

initVerdaWorld :: (MonadIO m, VerdaWorld w m) => SystemT w m ()
initVerdaWorld =
    set global =<< mkControlState

addSpriteRenderExtension :: VerdaWorld w IO => (RenderSpriteFn -> V2 Double -> Int -> SystemT w IO Int) -> SystemT w IO ()
addSpriteRenderExtension systemExtension = do
    world <- ask
    RenderExtensions spriteExts rendererExts <- get global
    let extension = SpriteRenderExtension $ \renderSprite camPos idx -> runWith world (systemExtension renderSprite camPos idx)
    global $= RenderExtensions (Vec.snoc spriteExts extension) rendererExts

addRendererExtension :: VerdaWorld w IO => (SDL.Renderer -> LogicToRenderFn -> V2 Double -> SystemT w IO ()) -> SystemT w IO ()
addRendererExtension systemExtension = do
    world <- ask
    RenderExtensions spriteExts rendererExts <- get global
    let extension = RendererExtension $ \renderer logicToRender camPos ->
            runWith world (systemExtension renderer logicToRender camPos)
    global $= RenderExtensions spriteExts (Vec.snoc rendererExts extension)
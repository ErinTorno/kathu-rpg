module Verda.Graphics.Components
    ( BackgroundColor(..)
    , Camera(..)
    , LogicToRenderFn
    , RendererExtension(..)
    , RenderExtensions(..)
    , RenderSpriteFn
    , Resolution(..)
    , SpriteRenderExtension(..)
    , Tint(..)
    , defaultCamera
    , noTint
    -- Re-exported
    , FontCache
    , Sprite
    , SpriteManager
    ) where

import           Apecs
import           Data.Aeson
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vec
import           Linear.V2
import qualified SDL

import           Verda.Graphics.Color
import           Verda.Graphics.Fonts         (FontCache)
import           Verda.Graphics.SpriteManager (SpriteManager)
import           Verda.Graphics.Sprites       (Sprite)

newtype BackgroundColor = BackgroundColor {unBackgroundColor :: Color}
instance Semigroup BackgroundColor where (<>) = mappend
instance Monoid BackgroundColor where mempty = BackgroundColor black
instance Component BackgroundColor where type Storage BackgroundColor = Global BackgroundColor

newtype Camera = Camera {zoom :: Double}
instance Component Camera where type Storage Camera = Unique Camera

defaultCamera :: Camera
defaultCamera = Camera 1

-- | buffer index -> sprite -> position -> sprite tint -> next buffer index
type RenderSpriteFn = Int -> Sprite -> V2 Double -> Tint -> IO Int

type LogicToRenderFn = V2 Double -> V2 Double

-- | sprite render function -> camera position -> first buffer index -> buffer index after operations
newtype SpriteRenderExtension = SpriteRenderExtension (RenderSpriteFn -> V2 Double -> Int -> IO Int)

-- | renderer -> logic pos to screen converter -> first buffer index -> buffer index after operations
newtype RendererExtension = RendererExtension (SDL.Renderer -> LogicToRenderFn -> V2 Double -> IO ())

data RenderExtensions = RenderExtensions
    { spriteExtensions   :: !(Vector SpriteRenderExtension)
    , rendererExtensions :: !(Vector RendererExtension)
    }

instance Semigroup RenderExtensions where (<>) = mappend
instance Monoid RenderExtensions where mempty = RenderExtensions Vec.empty Vec.empty
instance Component RenderExtensions where type Storage RenderExtensions = Global RenderExtensions

newtype Resolution = Resolution {unResolution :: V2 Int}
instance Semigroup Resolution where (<>) = mappend
instance Monoid Resolution where mempty = Resolution (V2 0 0)
instance Component Resolution where type Storage Resolution = Global Resolution

newtype Tint = Tint {unTint :: Color} deriving (Show, Eq)
instance Component Tint where type Storage Tint = Map Tint

instance FromJSON Tint where
    parseJSON v = Tint <$> parseJSON v

noTint :: Tint
noTint = Tint white
module Verda.Graphics.Components
    ( BackgroundColor(..)
    , Camera(..)
    , Resolution(..)
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
import           Linear.V2

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
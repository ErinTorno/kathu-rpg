module Verda.Graphics.Components
    ( BackgroundColor(..)
    -- Re-exported
    , FontCache
    , SpriteID
    , SpriteManager
    ) where

import           Apecs

import           Verda.Graphics.Color
import           Verda.Graphics.Fonts         (FontCache)
import           Verda.Graphics.SpriteManager (SpriteManager)
import           Verda.Graphics.Sprites       (SpriteID)

newtype BackgroundColor = BackgroundColor {unBackgroundColor :: Color}

instance Semigroup BackgroundColor where (<>) = mappend
instance Monoid BackgroundColor where mempty = BackgroundColor black
instance Component BackgroundColor where type Storage BackgroundColor = Global BackgroundColor
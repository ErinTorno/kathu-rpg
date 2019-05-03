{-# LANGUAGE DeriveGeneric #-}

module Kathu.Graphics.Palette where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CInt)
import Kathu.Graphics.Color
import Linear.V4 (V4(..))
import qualified SDL

type Filter = Color -> Color

-- for some reason, this always returns Nothing; will need to check later and see why formatPalette is missing; maybe format issue?
paletteFromSurface :: MonadIO m => SDL.Surface -> m (Maybe SDL.Palette)
paletteFromSurface sur = SDL.surfaceFormat sur >>= SDL.formatPalette

replaceColor :: MonadIO m => Color -> Color -> SDL.Palette -> m ()
replaceColor (Color src) (Color rep) pal = index >>= maybe (pure ()) replace
    where index :: MonadIO m => m (Maybe Int)
          index = (join . fmap (V.elemIndex src)) <$> SDL.paletteColors pal
          replace :: MonadIO m => Int -> m ()
          replace = SDL.setPaletteColors pal (V.singleton rep) . fromIntegral
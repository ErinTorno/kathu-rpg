{-# LANGUAGE OverloadedStrings #-}

module Kathu.SDLCommon where

import qualified SDL

import Data.Word
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

withSDL :: MonadIO m => m a -> m ()
withSDL op = do SDL.initialize []
                void op
                SDL.quit

withWindow :: MonadIO m => Text -> SDL.V2 Word32 -> Maybe SDL.OpenGLConfig -> (SDL.Window -> m a) -> m ()
withWindow title (SDL.V2 w h) glconfig op = do
    window <- SDL.createWindow title $ SDL.defaultWindow {SDL.windowOpenGL = glconfig, SDL.windowInitialSize = SDL.V2 (fromIntegral w) (fromIntegral h)}
    SDL.showWindow window
    void . op $ window
    SDL.destroyWindow window

renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow window screen image = SDL.surfaceBlit image Nothing screen Nothing >> SDL.updateWindowSurface window
                             
isOpen :: Maybe SDL.Event -> Bool
isOpen = maybe True (not . isQuitEvent)

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _ = False

conditionalRun :: MonadIO m => m a -> Bool -> m Bool
conditionalRun f True  = True <$ f
conditionalRun _ False = pure False

-- working with SDL data types

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle topLeft size
    where topLeft = SDL.P $ SDL.V2 x y
          size    = SDL.V2 w h

shiftRect :: Num a => a -> a -> SDL.Rectangle a -> SDL.Rectangle a
shiftRect dx dy (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)) = mkRect (x + dx) (y + dy) w h

mapRectDim :: Num a => (a -> a) -> SDL.Rectangle a -> SDL.Rectangle a
mapRectDim fn (SDL.Rectangle p (SDL.V2 w h)) = SDL.Rectangle p $ SDL.V2 (fn w) (fn h)

rectWidth :: SDL.Rectangle a -> a
rectWidth (SDL.Rectangle _ (SDL.V2 w _)) = w

rectHeight :: SDL.Rectangle a -> a
rectHeight (SDL.Rectangle _ (SDL.V2 _ h)) = h

-- Vector

zeroV2 :: Num a => SDL.V2 a
zeroV2 = SDL.V2 0 0

zeroV3 :: Num a => SDL.V3 a
zeroV3 = SDL.V3 0 0 0
{-# LANGUAGE OverloadedStrings #-}

module Kathu.App.SDLCommon where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign.C.String
import qualified SDL
import qualified SDL.Internal.Types     as SDLIn.Types
import qualified SDL.Raw.Basic          as SDLRaw
import qualified SDL.Raw.Enum           as SDLRaw
import qualified SDL.Raw.Video          as SDLRaw
                             
isOpen :: Maybe SDL.Event -> Bool
isOpen = maybe True (not . isQuitEvent)

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _ SDL.QuitEvent) = True
isQuitEvent _ = False

conditionalRun :: MonadIO m => m a -> Bool -> m Bool
conditionalRun f True  = True <$ f
conditionalRun _ False = pure False

setWindowIcon :: MonadIO m => SDL.Window -> SDL.Surface -> m ()
setWindowIcon (SDLIn.Types.Window window) (SDL.Surface sur _) = SDLRaw.setWindowIcon window sur

setOnFocusMouseClickthrough :: MonadIO m => Bool -> m ()
setOnFocusMouseClickthrough isEnabled = void . liftIO .
    withCString "SDL_MOUSE_FOCUS_CLICKTHROUGH" $ \hintStr ->
        withCString (if isEnabled then "1" else "0") $ \enabledStr -> do
            r <- SDLRaw.setHintWithPriority hintStr enabledStr SDLRaw.SDL_HINT_OVERRIDE
            print r
            pure ()

-- working with SDL data types

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect = mkRectWith id

mkRectWith :: (a -> b) -> a -> a -> a -> a -> SDL.Rectangle b
mkRectWith f x y w h = SDL.Rectangle topLeft size
    where topLeft = SDL.P $ SDL.V2 (f x) (f y)
          size    = SDL.V2 (f w) (f h)

shiftRect :: Num a => a -> a -> SDL.Rectangle a -> SDL.Rectangle a
shiftRect dx dy (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)) = mkRect (x + dx) (y + dy) w h

mapRectDim :: Num a => (a -> a) -> SDL.Rectangle a -> SDL.Rectangle a
mapRectDim fn (SDL.Rectangle p (SDL.V2 w h)) = SDL.Rectangle p $ SDL.V2 (fn w) (fn h)

rectWidth :: SDL.Rectangle a -> a
rectWidth (SDL.Rectangle _ (SDL.V2 w _)) = w

rectHeight :: SDL.Rectangle a -> a
rectHeight (SDL.Rectangle _ (SDL.V2 _ h)) = h
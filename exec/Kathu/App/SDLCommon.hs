module Kathu.App.SDLCommon where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.Storable (peek)
import qualified SDL
import qualified SDL.Internal.Types as SDLIn.Types
import qualified SDL.Raw.Video as SDLRaw.Video
                             
isOpen :: Maybe SDL.Event -> Bool
isOpen = maybe True (not . isQuitEvent)

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _ SDL.QuitEvent) = True
isQuitEvent _ = False

conditionalRun :: MonadIO m => m a -> Bool -> m Bool
conditionalRun f True  = True <$ f
conditionalRun _ False = pure False

setWindowIcon :: MonadIO m => SDL.Window -> SDL.Surface -> m ()
setWindowIcon (SDLIn.Types.Window window) (SDL.Surface sur _) = SDLRaw.Video.setWindowIcon window sur

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
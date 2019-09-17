{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Kathu.App.Graphics.Drawing where
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import qualified SDL

import Kathu.App.Graphics.Image (ImageID)
import Kathu.App.Graphics.ImageManager
import qualified Kathu.App.SDLCommon as SDLC
import Kathu.Graphics.Drawable

getImageID :: RenderSprite ImageID -> ImageID
getImageID (RSStatic (StaticSprite img _)) = img
getImageID (RSAnimated anim) = animAtlas . animation $ anim

mkRenderRect :: Float -> V2 Float -> Float -> V2 Float -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRect bleed (V2 shiftX shiftY) scale (V2 x y) (SDL.Rectangle _ (V2 w h)) = SDLC.mkRectWith round x' y' (bleed * scale * fromIntegral w) (bleed * scale * fromIntegral h)
    where x' = x - scale * 0.5 * fromIntegral w + shiftX
          y' = y - scale * fromIntegral h + shiftY

mkRenderRectNoCenter :: Float -> V2 Float -> Float -> V2 Float -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRectNoCenter bleed (V2 shiftX shiftY) scale (V2 x y) (SDL.Rectangle _ (V2 w h)) = SDLC.mkRectWith round x' y' (bleed * scale * fromIntegral w) (bleed * scale * fromIntegral h)
    where x' = x - scale * shiftX
          y' = y - scale * fromIntegral h + shiftY

blitRenderSprite :: MonadIO m => ImageManager -> (SDL.Rectangle CInt -> SDL.Rectangle CInt) -> RenderSprite ImageID -> SDL.Surface -> m SDL.Surface
blitRenderSprite im mkRect ren scr = blit ren >> pure scr
    where draw bnd img = SDL.surfaceBlitScaled img (Just bnd) scr (Just . mkRect $ bnd)
          blit (RSStatic (StaticSprite !iid !bnd)) = fetchImage iid im >>= draw (SDL.Rectangle (SDL.P (V2 0 0)) bnd)
          blit dyn@(RSAnimated (AnimatedSprite {animation = anim})) = fetchImage (animAtlas anim) im >>= draw bounds
              where bounds = SDL.Rectangle (SDL.P boundsPos) boundsDim
                    (# boundsPos, boundsDim #) = currentBounds dyn
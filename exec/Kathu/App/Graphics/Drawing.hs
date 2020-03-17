{-# LANGUAGE BangPatterns  #-}
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
getImageID (RSStatic (StaticSprite !img _ _)) = img
getImageID (RSAnimated !anim) = animAtlas . animation $ anim

mkRenderRect :: (Floating a, RealFrac a) => a -> V2 a -> a -> V2 a -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRect !bleed (V2 !shiftX !shiftY) !scale (V2 !x !y) (SDL.Rectangle _ (V2 !w !h)) = SDLC.mkRectWith round x' y' (bleed * scale * fromIntegral w) (bleed * scale * fromIntegral h)
    where x' = x - scale * 0.5 * fromIntegral w + shiftX
          y' = y - scale * fromIntegral h + shiftY

mkRenderRectNoCenter :: (Floating a, RealFrac a) => a -> V2 a -> a -> V2 a -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRectNoCenter !bleed (V2 !shiftX !shiftY) !scale (V2 !x !y) (SDL.Rectangle _ (V2 !w !h)) = SDLC.mkRectWith round x' y' (bleed * scale * fromIntegral w) (bleed * scale * fromIntegral h)
    where x' = x - scale * shiftX
          y' = y - scale * fromIntegral h + shiftY

blitRenderSprite :: MonadIO m => SDL.Renderer -> ImageManager -> (SDL.Rectangle CInt -> SDL.Rectangle CInt) -> RenderSprite ImageID -> m ()
blitRenderSprite !renderer !im !mkRect !ren = blit ren
    where draw !srcBnd !destBnd tex = SDL.copy renderer tex srcBnd (Just . mkRect $ destBnd)
          blit (RSStatic (StaticSprite !iid !bnd _)) = mapM_ (draw Nothing (SDL.Rectangle (SDL.P (V2 0 0)) bnd)) $ fetchTextures iid im
          blit dyn@(RSAnimated AnimatedSprite {animation = anim}) = mapM_ (draw (Just bounds) bounds) $ fetchTextures (animAtlas anim) im
              where bounds = SDL.Rectangle (SDL.P boundsPos) boundsDim
                    (# boundsPos, boundsDim #) = currentBounds dyn
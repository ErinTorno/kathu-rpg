{-# LANGUAGE UnboxedTuples #-}

module Kathu.App.Graphics.Drawing where

import           Control.Monad.IO.Class          (MonadIO)
import           Foreign.C.Types                 (CInt)
import           Linear.V2                       (V2(..))
import qualified SDL

import           Kathu.App.Graphics.Image        (ImageID)
import           Kathu.App.Graphics.ImageManager
import           Kathu.Graphics.Drawable
import           Verda.Util.Types                (clampBetween)

-- the height of the screen in units; depending on screen size, more or less is included

minUnitsPerHeight :: Floating a => a
minUnitsPerHeight = 8.0

maxUnitsPerHeight :: Floating a => a
maxUnitsPerHeight = 14.0

pixelsForMinUnits :: Integral a => a
pixelsForMinUnits = 360

pixelsForMaxUnits :: Integral a => a
pixelsForMaxUnits = 1080

pixelsPerUnit :: Floating a => a
pixelsPerUnit = 16.0

cameraShiftUp :: Floating a => a
cameraShiftUp = 0.75

-- sprite dimensions are multiplied by this to prevent tiny streaks between adjacent sprites
edgeBleedScaling :: Floating a => a
edgeBleedScaling = 1.005

aspectRatio :: Floating a => V2 a -> a
aspectRatio (V2 x y) = x / y

getUnitsPerHeight :: (Integral i, Floating a) => i -> a
getUnitsPerHeight resY = minUnitsPerHeight + (maxUnitsPerHeight - minUnitsPerHeight) * pixMult
    where pixMult = fromIntegral (clampBetween pixelsForMinUnits pixelsForMaxUnits resY) / fromIntegral (pixelsForMaxUnits :: Int)

getScale :: Floating a => a -> a -> a -> a
getScale screenY unitsPerH zoomScale = screenY / (zoomScale * unitsPerH * pixelsPerUnit)

worldToScreenScale :: Floating a => V2 a -> a -> a -> a -> (V2 a -> V2 a)
worldToScreenScale screenDim scale camX camY = \pos -> halfScreenDim + ((*logicScale) <$> (pos - shiftedCamera))
    where -- we mult by this again to convert the 1-per-tile view of the entity-world into a N-pixels-per-tile view
          logicScale    = scale * pixelsPerUnit
          shiftedCamera = V2 camX (camY - cameraShiftUp)
          halfScreenDim = (*0.5) <$> screenDim

screenToWorldScale :: Floating a => V2 a -> a -> a -> a -> (V2 a -> V2 a)
screenToWorldScale screenDim scale camX camY = \pos -> ((/logicScale) <$> (pos - halfScreenDim)) + shiftedCamera
    where logicScale    = scale * pixelsPerUnit
          shiftedCamera = V2 camX (camY - cameraShiftUp)
          halfScreenDim = (*0.5) <$> screenDim

getImageID :: RenderSprite ImageID -> ImageID
getImageID (RSStatic (StaticSprite !img _ _)) = img
getImageID (RSAnimated !anim) = animAtlas . animation $ anim

mkRenderRect :: (Floating a, RealFrac a) => a -> a -> V2 a -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRect !bleed !scale (V2 !x !y) (SDL.Rectangle _ (V2 !w !h)) = mkRectWith round x' y' (bleed * scale * fromIntegral w) (bleed * scale * fromIntegral h)
    where x' = x - scale * 0.5 * fromIntegral w
          y' = y - scale * fromIntegral h

mkRenderRectNoCenter :: (Floating a, RealFrac a) => a -> a -> V2 a -> SDL.Rectangle CInt -> SDL.Rectangle CInt
mkRenderRectNoCenter !bleed !scale (V2 !x !y) (SDL.Rectangle _ (V2 !w !h)) = mkRectWith round x y' (bleed * scale * fromIntegral w) (bleed * scale * fromIntegral h)
    where y' = y - scale * fromIntegral h

blitRenderSprite :: MonadIO m => SDL.Renderer -> ImageManager -> (SDL.Rectangle CInt -> SDL.Rectangle CInt) -> RenderSprite ImageID -> m ()
blitRenderSprite !renderer !im !mkRect !ren = blit ren
    where draw !srcBnd !destBnd tex = SDL.copy renderer tex srcBnd (Just . mkRect $ destBnd)
          blit (RSStatic (StaticSprite !iid !bnd _)) = mapM_ (draw Nothing (SDL.Rectangle (SDL.P (V2 0 0)) bnd)) $ fetchTextures iid im
          blit dyn@(RSAnimated AnimatedSprite {animation = anim}) = mapM_ (draw (Just bounds) bounds) $ fetchTextures (animAtlas anim) im
              where bounds = SDL.Rectangle (SDL.P boundsPos) boundsDim
                    (# boundsPos, boundsDim #) = currentBounds dyn

-------------------------
-- SDL Data Type Utils --
-------------------------

mkRectWith :: (a -> b) -> a -> a -> a -> a -> SDL.Rectangle b
mkRectWith f x y w h = SDL.Rectangle topLeft size
    where topLeft = SDL.P $ SDL.V2 (f x) (f y)
          size    = SDL.V2 (f w) (f h)
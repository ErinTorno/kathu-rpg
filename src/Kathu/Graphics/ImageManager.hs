{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Kathu.Graphics.ImageManager
    ( ImageManager
    , defaultImageManager
    , mkImageManager
    , resetImageManager
    , fetchImage
    , backgroundColor
    , loadPalettes
    , setPalette
    , maxPalettes
    ) where

import Control.Lens
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.C.Types (CInt)
import Foreign.Ptr
import Foreign.Storable
import Kathu.Graphics.Color
import Kathu.Graphics.Drawable
import Kathu.Graphics.Palette
import qualified Kathu.SDLCommon as SDLC
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import qualified SDL
import qualified SDL.Raw.Types as SDLRaw

maxPalettes :: Num a => a
maxPalettes = 4

backgroundMask :: Color
backgroundMask = black

data SurfaceSet = SurfaceSet
    { _isActive :: Bool -- in future we can "drop" or unload 
    , _baseSurface :: SDL.Surface
    , _convSurfaces :: IOVector SDL.Surface
    }
makeLenses ''SurfaceSet

{-
-- considered type for using Storable Vector
-- not needed now, but maybe eventually
newtype SurfaceSet = SurfaceSet
    ( Ptr SDLRaw.Surface
    , IOVector (Ptr SDLRaw.Surface)
    ) deriving Storable
    -- not sure if managed or not (Surface has Just (IOVector Word8); if not, we can fully store it; otherwise we will need more
-}

data ImageManager = ImageManager
    { _currentSet :: Int
    , _backgrounds :: Vector Color
    , _surfaceSets :: IOVector SurfaceSet
    }
makeLenses ''ImageManager

defaultImageManager :: ImageManager
defaultImageManager = error "Attempted to access uninitialized ImageManager"

mkImageManager :: Vector SDL.Surface -> IO ImageManager
mkImageManager !surs = pure . ImageManager 0 (Vec.singleton (black)) =<< Vec.thaw =<< mapM mkSet surs
        where mkSet :: SDL.Surface -> IO SurfaceSet
              mkSet sur  = pure . SurfaceSet True sur =<< MVec.replicateM maxPalettes (dupSur sur)
              dupSur sur = SDL.surfaceFormat sur >>= SDL.convertSurface sur

resetImageManager :: MonadIO m => ImageManager -> m ImageManager
resetImageManager (ImageManager _ bkg sets) = (mVecMapM_ resetIndiv sets) >> (pure $ ImageManager 0 bkg sets)
    where resetIndiv (SurfaceSet _ base surs) = mVecMapM_ (copySurfaceOver base) surs
    
setPalette :: Int -> ImageManager -> ImageManager
setPalette = set currentSet

fetchImage :: MonadIO m => ImageID -> ImageManager -> m SDL.Surface
fetchImage (ImageID iid) (ImageManager curS _ surSets) = liftIO $ MVec.read surSets iid >>= (flip MVec.read) curS . view convSurfaces

backgroundColor :: ImageManager -> Color
backgroundColor (ImageManager i bkg _) = bkg Vec.! i

loadPalettes :: MonadIO m => Vector Palette -> ImageManager -> m ImageManager
loadPalettes thms im@(ImageManager _ _ sets) = (resetImageManager im) >>= ((mVecMapM_ resetIndv sets)>>) . pure . set backgrounds (background <$> thms)
    where resetIndv (SurfaceSet _ base surs) = copySurface 0 surs
          copySurface !i surs | i == thmLen = pure ()
                              | otherwise   = do
                                  surface <- (liftIO . MVec.read surs) i
                                  let theme = thms Vec.! i
                                  replaceSurfaceColor backgroundMask (background theme) surface
          thmLen = let l = Vec.length thms in if l > maxPalettes then failure l else l
          failure l = (error . concat) ["Attempted to load theme set past max limit (max: ", show maxPalettes, " given length: ", show l, ")"]

mVecMapM_ :: MonadIO m => (a -> m b) -> IOVector a -> m ()
mVecMapM_ f v = go 0
    where len = MVec.length v
          go !i | i == len  = pure ()
                | otherwise = (liftIO . MVec.read v) i >>= f >> go (i + 1)

-------------
-- Warning --
-------------
-- The following section is rather dangerous, and makes use of SDL.Raw and Ptrs

-- for some reason, this always returns Nothing; will need to check later and see why formatPalette is missing; maybe format issue?
paletteFromSurface :: MonadIO m => SDL.Surface -> m (Maybe SDL.Palette)
paletteFromSurface sur = SDL.surfaceFormat sur >>= SDL.formatPalette

replacePaletteColor :: MonadIO m => Color -> Color -> SDL.Palette -> m ()
replacePaletteColor (Color src) (Color rep) pal = index >>= maybe (pure ()) replace
    where index :: MonadIO m => m (Maybe Int)
          index = (join . fmap (V.elemIndex src)) <$> SDL.paletteColors pal
          replace :: MonadIO m => Int -> m ()
          replace = SDL.setPaletteColors pal (V.singleton rep) . fromIntegral

-- | Copies a surface over another surface; both must have same size and pixel format, with 32 bits per pixel
copySurfaceOver :: MonadIO m => SDL.Surface -> SDL.Surface -> m ()
copySurfaceOver !sur !tar = do
    SDL.lockSurface sur
    SDL.lockSurface tar

    format <- SDLC.surfacePixelFormat sur
    (V2 w h) <- SDL.surfaceDimensions sur
    do (V2 wtar htar) <- SDL.surfaceDimensions tar
       tarFormat <- SDLC.surfacePixelFormat tar
       if SDLRaw.pixelFormatBytesPerPixel format /= 4 then
           error "Attempted to copy colors in a surface that doesn't support all color channels" 
       else if format /= tarFormat then
           error "Attempted to copy colors between two surfaces with different pixel formats"
       else if w /= wtar || h /= htar then
           error "Attempted to copy between surfaces with different dimensions"
       else pure ()

    surPixels <- SDL.surfacePixels sur
    tarPixels <- SDL.surfacePixels tar
    let len = fromIntegral $ w * h * fromIntegral (SDLRaw.pixelFormatBytesPerPixel format)
        copyAt !i | i >= len  = pure ()
                  | otherwise = ((peekByteOff surPixels i) :: IO Word8) >>= pokeByteOff tarPixels i >> copyAt (i + 1)
    liftIO $ copyAt 0

    SDL.unlockSurface sur
    SDL.unlockSurface tar

-- | Replaces all pixels in a surface of a certain color with another; each pixel in the format must be 32 bits
replaceSurfaceColor :: MonadIO m => Color -> Color -> SDL.Surface -> m ()
replaceSurfaceColor !srcColor !repColor sur = do
    SDL.lockSurface sur

    format <- SDLC.surfacePixelFormat sur
    if SDLRaw.pixelFormatBytesPerPixel format /= 4 then
        error "Attempted to replace colors in a surface that doesn't support all color channels" 
    else pure ()

    (V2 w h) <- SDL.surfaceDimensions sur
    pixels   <- castPtr <$> SDL.surfacePixels sur
    let src = colorToWord32 format srcColor
        rep = colorToWord32 format repColor
        len = fromIntegral (w * h)
        replaceAtI !i | i >= len  = pure ()
                      | otherwise = do
                          c <- (peekElemOff pixels i) :: IO Word32
                          if c == src then pokeElemOff pixels i rep else pure ()
                          replaceAtI (i + 1)

    liftIO $ replaceAtI 0
    SDL.unlockSurface sur

colorToWord32 :: SDLRaw.PixelFormat -> Color -> Word32
colorToWord32 format (Color (V4 !r !g !b !a)) = gp SDLRaw.pixelFormatRMask r .|. gp SDLRaw.pixelFormatGMask g .|. gp SDLRaw.pixelFormatBMask b .|. gp SDLRaw.pixelFormatAMask a
    where gp f b = let w = fromIntegral b
                   in  f format .&. (w .|. (w `shift` 8) .|. (w `shift` 16) .|. (w `shift` 24))
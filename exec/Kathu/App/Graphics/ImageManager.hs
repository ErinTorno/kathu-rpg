{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- This file contains a lot of "unused" code that clarifies meaning, like names in records

{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}

module Kathu.App.Graphics.ImageManager
    ( ImageManager
    , defaultImageManager
    , mkImageManager
    , runImageManager
    , nextPaletteManager
    , setPaletteIdx
    , setPaletteManager
    , fetchTextures
    , backgroundColor
    , loadPalettes
    , currentPalette
    , availablePaletteCount
    , setWindowIcon
    ) where

import           Apecs                       (SystemT, global)
import qualified Apecs
import           Control.Lens
import           Control.Monad               (foldM, foldM_, join, void, when)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Bits
import qualified Data.Map                     as Map
import qualified Data.Vector                  as Vec
import qualified Data.Vector.Unboxed          as UVec
import qualified Data.Vector.Unboxed.Mutable  as UMVec
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Linear.V2                    (V2(..))
import           Linear.V3                    (V3(..))
import           Linear.V4                    (V4(..))
import           SDL                          (($=))
import qualified SDL
import qualified SDL.Internal.Types           as SDLInternal
import qualified SDL.Raw.Video                as SDLVRaw
import qualified SDL.Raw.Types                as SDLRaw

import           Kathu.App.Graphics.Image     (ImageID(..))
import           Kathu.Entity.Time
import           Kathu.Graphics.Color
import           Kathu.Graphics.Palette
import           Verda.Util.Apecs
import           Verda.Util.Types             (Identifier, IDMap)

-- Warning: most things in this class are private, and as it makes use of rather unsafe operations and mutability

-- | Refers to the base object in the palette manager map, where there are no filters and just a black background
noPalette :: Identifier
noPalette = ""

backgroundMask :: Color
backgroundMask = black

alphaColor :: Color
alphaColor = mkColor 0 0 0 0

-- | By default, all ImageSets will allocate room for this many palettes; they will expand dynamically though if more are provided
basePaletteMaximum :: Integral a => a
basePaletteMaximum = 256

data TextureSet = TextureSet
    { textures    :: Vec.Vector SDL.Texture -- each color in image other than alpha is split into separate texture
    , basePalette :: UVec.Vector Color      -- the default palette; not used in rendering, only when creating new palettes, never for rendering or updating textures
    , palettes    :: UMVec.IOVector Color   -- all available colors to textureColorMod; palette i starts at index i*(size textures)
    }

data ImageManager = ImageManager
    { _currentSetIdx     :: Int
    , _paletteSetCount   :: Int -- Not including base idx 0 colors
    , _currentManagerIdt :: Identifier
    , _paletteManagers   :: IDMap PaletteManager
    , _backgrounds       :: UVec.Vector Color
    , _textureSets       :: Vec.Vector TextureSet
    }
makeLenses ''ImageManager

defaultImageManager :: ImageManager
defaultImageManager = error "Attempted to access uninitialized ImageManager"

mkImageManager :: SDL.Renderer -> Vec.Vector SDL.Surface -> IO ImageManager
mkImageManager renderer surfaces = do
    let mkSet :: SDL.Surface -> IO TextureSet
        mkSet surface = do
            nBasePalette   <- mkPalette surface
            surfaceFormat <- SDL.surfaceFormat surface
            -- a surface target to copy select pixel information over
            copySurface   <- SDL.convertSurface surface surfaceFormat
            let mkTex col  = copyColorMaskOverSurface col surface copySurface >> SDL.createTextureFromSurface renderer copySurface
            -- for each color in palette, we create a separate texture with only pixel information 
            texs          <- Vec.fromList <$> mapM mkTex nBasePalette
            paletteVec    <- UMVec.unsafeNew (length nBasePalette * basePaletteMaximum)
            foldM_ (\i col -> UMVec.write paletteVec i col >> pure (i + 1)) 0 nBasePalette
            pure $ TextureSet texs (UVec.fromList nBasePalette) paletteVec
        basePalManagers = Map.singleton noPalette (staticManager 0)

    ImageManager 0 1 noPalette basePalManagers (UVec.singleton black) <$> mapM mkSet surfaces

runImageManager :: forall w m. (MonadIO m, ReadWriteEach w m '[ImageManager, PaletteManager, RenderTime])
                => SystemT w m ()
runImageManager = do
    mngr@(PaletteManager _ runPal) <- Apecs.get global
    runPal setPaletteIdx (void . setPaletteManager) mngr

nextPaletteManager :: ImageManager -> Identifier
nextPaletteManager im =
    case im^.paletteManagers.to (Map.lookupGT (im^.currentManagerIdt)) of
        (Just (idt, _)) -> idt
        Nothing         -> fst (im^.paletteManagers.to Map.findMin)

setPaletteIdx :: forall w m. (MonadIO m, ReadWriteEach w m '[ImageManager, PaletteManager, RenderTime])
              => Int -> SystemT w m ()
setPaletteIdx i = Apecs.get global >>= setPalette i >>= Apecs.set global >> pure ()

-- | Changes palette manager to the one specified; False will be returned if there was no matching palette manager
setPaletteManager :: forall w m. (MonadIO m, ReadWriteEach w m '[ImageManager, PaletteManager, RenderTime])
                  => Identifier -> SystemT w m Bool
setPaletteManager idt = do
    manager <- Apecs.get global
    case manager^.paletteManagers.to (Map.lookup idt) of
        Nothing        -> pure False
        (Just newMngr) -> do
            initManager newMngr setPaletteIdx newMngr
            Apecs.set global newMngr
            manager' <- Apecs.get global
            Apecs.set global (set currentManagerIdt idt manager')
            pure True

-- warning: mutates internal ImageManager TextureSet states, and will error if given out-of-bounds palette index
setPalette :: MonadIO m => Int -> ImageManager -> m ImageManager
setPalette newSet = (\im -> liftIO (mapM_ updateTex . view textureSets $ im) >> pure im) . set currentSetIdx newSet . check
    where updateTex :: TextureSet -> IO ()
          updateTex (TextureSet texs _ pals) = foldM_ (applyColorMod pals) (newSet * Vec.length texs) texs
          applyColorMod pals i tex = do
              (Color (V4 r g b a)) <- UMVec.unsafeRead pals i
              SDL.textureColorMod tex $= V3 r g b
              SDL.textureAlphaMod tex $= a
              pure (i + 1)
          check im | newSet >= im^.paletteSetCount = error . concat $ ["Attempted to set ImageManager palette to index ", show newSet, " when there are only ", im^.paletteSetCount.to show, " available palettes"]
                   | otherwise                     = im

backgroundColor :: ImageManager -> Color
backgroundColor im = (im^.backgrounds) UVec.! (im^.currentSetIdx)

currentPalette :: ImageManager -> Int
currentPalette im = im^.currentSetIdx

availablePaletteCount :: ImageManager -> Int
availablePaletteCount im = im^.paletteSetCount

fetchTextures :: ImageID -> ImageManager -> Vec.Vector SDL.Texture
fetchTextures (ImageID iid) = textures . (Vec.!iid) . view textureSets

loadPalettes :: MonadIO m => IDMap Palette -> ImageManager -> m ImageManager
loadPalettes newPalettes im = liftIO (mapM updateSet . view textureSets $ im)
                          >>= setPalette (min 1 paletteCount) . ImageManager 0 (paletteCount + 1) noPalette newManagers newBackgrounds
    where paletteCount = UVec.length newBackgrounds
          interpolatedBackgrounds :: [(Palette, [Color])]
          interpolatedBackgrounds = (\p -> (p,) . allBackgrounds $ p) <$> Map.elems newPalettes

          backgroundVecs = Vec.fromList . cons (Vec.singleton black) . fmap (Vec.fromList . snd) $ interpolatedBackgrounds
          newBackgrounds = UVec.convert . join $ backgroundVecs
          newManagers :: IDMap PaletteManager
          newManagers = Map.fromList . fst . Vec.foldl' mkManager ([], 0) . Vec.zip assocVec $ backgroundVecs
              where assocVec = Vec.fromList . cons (noPalette, emptyPalette) . Map.assocs $ newPalettes
                    -- We know that a palette will take up a number of slots equal to its background frame count
                    mkManager (acc, idx) ((pid, p), bkgs) = let endIdx = idx + Vec.length bkgs - 1
                                                             in ((pid, managerFromPalette idx endIdx p):acc, endIdx + 1)
          updateSet :: TextureSet -> IO TextureSet
          updateSet (TextureSet texs basePal pals) = do
              let colorsPerSet = UVec.length basePal
              pals' <- growIfNeeded colorsPerSet pals
              foldM_ (writeNewCols basePal pals') colorsPerSet interpolatedBackgrounds
              pure (TextureSet texs basePal pals')
          -- If shader is Nothing, we write base palette into slots; otherwise we write the value yielded from passing each color into the shader function
          writeNewCols :: UVec.Vector Color -> UMVec.IOVector Color -> Int -> (Palette, [Color]) -> IO Int
          writeNewCols basePal pals i (pal, bkgs) = case pal of
              (SPalette (StaticPalette bkg (Shader shdr))) -> UVec.foldM (writeFrame pals bkg shdr) i basePal
              (APalette animPalette)                       -> fmap (const $ i + length bkgs * UVec.length basePal)
                                                            . Vec.foldM_ (\off cols -> writeAnimFrame (UVec.length basePal) pals (i + off) cols >> pure (off + 1)) 0
                                                            . Vec.map (applyAnimPalette bkgs animPalette)
                                                            . Vec.convert
                                                            $ basePal
          applyAnimPalette :: [Color] -> AnimatedPalette -> Color -> [Color]
          applyAnimPalette bkgs pal col | col == backgroundMask = bkgs
                                        | otherwise             = applyAnimatedPalette pal col
          writeAnimFrame :: Int -> UMVec.IOVector Color -> Int -> [Color] -> IO Int
          writeAnimFrame count pals = foldM (\i col -> UMVec.unsafeWrite pals i col >> pure (i + count))
          writeFrame pals bkg shdr idx col = UMVec.unsafeWrite pals idx (getColor shdr bkg col) >> pure (idx + 1)
          -- if the base color matches our mask, we instead use the background color instead
          getColor f bkg col = if col == backgroundMask then bkg else f col
          -- If we are trying to add more shaders than the current vectors can hold, we grow them to fit
          -- The amount of room we add is basePaletteMaximum, unless more is needed
          growIfNeeded n pals | reqSlots > palsLen = UMVec.unsafeGrow pals (max (reqSlots - palsLen) basePaletteMaximum)
                              | otherwise          = pure pals -- just good friends :)
              where reqSlots = n * (paletteCount + 1)
                    palsLen  = UMVec.length pals

-- Not strictly ImageManager related, but this module is filled with almost all of the ugly dangerous SDL rendering functions
setWindowIcon :: MonadIO m => SDL.Window -> SDL.Surface -> m ()
setWindowIcon (SDLInternal.Window window) (SDL.Surface surPtr _) = SDLVRaw.setWindowIcon window surPtr

-------------
-- Warning --
-------------

-- Extra warning level: this section makes use of raw pointers and casting

mkPalette :: MonadIO m => SDL.Surface -> m [Color]
mkPalette sur = do
    SDL.lockSurface sur
    
    format   <- rawSurfaceFormat sur
    (V2 w h) <- SDL.surfaceDimensions sur
    when (SDLRaw.pixelFormatBytesPerPixel format /= 4) $
        error "Attempted to find palette in a surface that doesn't support all color channels"

    pixels   <- castPtr <$> SDL.surfacePixels sur
    let len = fromIntegral $ w * h
        checkColor acc i
            | i >= len  = pure acc
            | otherwise = do
                word <- peekElemOff pixels i :: IO Word32
                let col = word32ToColor format word
                if col `elem` acc then
                    checkColor acc (i + 1)
                else
                    checkColor (col:acc) (i + 1)
        removeUnn (Color (V4 _ _ _ a)) = a /= 0
    colorList <- liftIO . fmap (filter removeUnn) . checkColor [] $ 0
    SDL.unlockSurface sur
    pure colorList

-- | Copies pixels in a target Surface that match the given color over to another Surface as white; other pixels will be fully transparent
-- | Both must have same size and pixel format, with 32 bits per pixel
copyColorMaskOverSurface :: MonadIO m => Color -> SDL.Surface -> SDL.Surface -> m ()
copyColorMaskOverSurface col sur tar = do
    SDL.lockSurface sur
    SDL.lockSurface tar

    format <- rawSurfaceFormat sur
    (V2 w h) <- SDL.surfaceDimensions sur
    do (V2 wtar htar) <- SDL.surfaceDimensions tar
       tarFormat <- rawSurfaceFormat tar
       if SDLRaw.pixelFormatBytesPerPixel format /= 4 then
           error "Attempted to copy colors in a surface that doesn't support all color channels" 
       else if format /= tarFormat then
           error .concat $ ["Attempted to copy colors between two surfaces with different pixel formats (", show format, ", ", show tarFormat, ")"]
       else if w /= wtar || h /= htar then
           error "Attempted to copy between surfaces with different dimensions"
       else pure ()
 
    let whitePixel  = colorToWord32 format white
        alphaPixel  = colorToWord32 format alphaColor   
        targetPixel = colorToWord32 format col
    surPixels <- castPtr <$> SDL.surfacePixels sur
    tarPixels <- castPtr <$> SDL.surfacePixels tar
    let len = fromIntegral $ w * h
        copyAt i | i >= len  = pure ()
                 | otherwise = do
                     pixel <- peekElemOff surPixels i :: IO Word32
                     if pixel == targetPixel then
                        pokeElemOff tarPixels i whitePixel
                     else
                        pokeElemOff tarPixels i alphaPixel
                     copyAt (i + 1)
    liftIO $ copyAt 0

    SDL.unlockSurface sur
    SDL.unlockSurface tar

rawSurfaceFormat :: MonadIO m => SDL.Surface -> m SDLRaw.PixelFormat
rawSurfaceFormat (SDL.Surface sur _) = liftIO (peek . SDLRaw.surfaceFormat =<< peek sur)

colorToWord32 :: SDLRaw.PixelFormat -> Color -> Word32
colorToWord32 format (Color (V4 r g b a)) = gp SDLRaw.pixelFormatRMask r .|. gp SDLRaw.pixelFormatGMask g .|. gp SDLRaw.pixelFormatBMask b .|. gp SDLRaw.pixelFormatAMask a
    where gp f byte = let w = fromIntegral byte
                       in f format .&. (w .|. (w `shift` 8) .|. (w `shift` 16) .|. (w `shift` 24))

word32ToColor :: SDLRaw.PixelFormat -> Word32 -> Color
word32ToColor format word = mkColor (getComp SDLRaw.pixelFormatRMask) (getComp SDLRaw.pixelFormatGMask) (getComp SDLRaw.pixelFormatBMask) (getComp SDLRaw.pixelFormatAMask)
    where getComp mask = let c = mask format .&. word -- we get just the component part
                         in fromIntegral $ (255 .&. c) .|. (255 .&. (c `shift` (-8))) .|. (255 .&. (c `shift` (-16))) .|. (255 .&. (c `shift` (-24))) -- fill up word with it, then cast to Word8 to drop out else

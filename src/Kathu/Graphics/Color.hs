{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Kathu.Graphics.Color where

import Data.Fixed (mod')
import Data.Word
import GHC.Generics
import Linear.V4 (V4(..))
import Numeric (readHex, showHex)

newtype Color = Color (V4 Word8) deriving (Eq, Generic)

instance Show Color where
    show (Color (V4 r g b a)) = ('#':) . showHex r . showHex g . showHex b . showHex a $ ""
instance Read Color where
    readsPrec _ str =
        let pair a b = fst . head . readHex $ (a:b:[])
            mkParse r g b a = pure . (, []) $ mkColor r g b a
        in case str of
            ('#':r1:r2:g1:g2:b1:b2:a1:a2:[]) -> mkParse (pair r1 r2) (pair g1 g2) (pair b1 b2) (pair a1 a2)
            ('#':r1:r2:g1:g2:b1:b2:[])       -> mkParse (pair r1 r2) (pair g1 g2) (pair b1 b2) 0
            _                                 -> error "Unable to parse color format"

mkColor :: Word8 -> Word8 -> Word8 -> Word8 -> Color
mkColor r g b a = Color $ V4 r g b a

red :: Color -> Word8
red   (Color (V4 r _ _ _)) = r
green :: Color -> Word8
green (Color (V4 _ g _ _)) = g
blue :: Color -> Word8
blue  (Color (V4 _ _ b _)) = b
alpha :: Color -> Word8
alpha (Color (V4 _ _ _ a)) = a

data HSVColor = HSVColor
    { hue        :: Float
    , saturation :: Float
    , value      :: Float
    , hsvAlpha   :: Float
    } deriving (Show, Eq, Generic)

--------------------
-- Manipulate RGB --
--------------------

desaturate :: Color -> Color
desaturate (Color (V4 r g b a)) = Color $ V4 luminosity luminosity luminosity a
    where -- no set reason other than these values appear nicely to the human eye
          luminosity = floor $ 0.21 * fromIntegral r + 0.72 * fromIntegral g + 0.07 * fromIntegral b

desaturateBy :: Float -> Color -> Color
desaturateBy percent color = blendColor percent (desaturate color) color

-- ratio is ratio of 2nd color to 1st; so 1.0 is only 2nd color, etc.
blendColor :: Float -> Color -> Color -> Color
blendColor ratio (Color (V4 r1 g1 b1 a1)) (Color (V4 r2 g2 b2 a2)) = Color $ V4 (blEach r1 r2) (blEach g1 g2) (blEach b1 b2) (blEach a1 a2) 
    where blEach x y = floor $ (1.0 - ratio) * fromIntegral x + ratio * fromIntegral y 

--------------------
-- Manipulate HSV --
--------------------

getHSVDifference :: HSVColor -> HSVColor -> HSVColor
getHSVDifference primary col = HSVColor {hue = difVal hue, saturation = difVal saturation, value = difVal value, hsvAlpha = difVal hsvAlpha}
    where difVal getter = getter primary - getter col

applyHSVDifference :: HSVColor -> HSVColor -> HSVColor
applyHSVDifference template col = HSVColor {hue = (restrictHue . addVal) hue, saturation = addVal saturation, value = addVal value, hsvAlpha = addVal hsvAlpha}
    where addVal getter = getter col + getter template

-- Hue should always be between 0 and 360
restrictHue :: Float -> Float
restrictHue h | h < 0.0   = restrictHue (h + 360.0)
              | h > 360.0 = restrictHue (h - 360.0)
              | otherwise = h

shiftHue :: Float -> HSVColor -> HSVColor
shiftHue p (HSVColor h s v a) = HSVColor h' s v a
            where h' = (h + p * 180.0) `mod'` 360.0

invertHue :: HSVColor -> HSVColor
invertHue = shiftHue 0.5 -- shift to exactly across color wheel

-- Conversion functions

fromHSVFunction :: (HSVColor -> HSVColor) -> (Color -> Color)
fromHSVFunction f = fromHSV . f . fromRGB

fromRGBFunction :: (Color -> Color) -> (HSVColor -> HSVColor)
fromRGBFunction f = fromRGB . f . fromHSV

-- Converts an RGB color into an HSV color
fromRGB :: Color -> HSVColor
fromRGB (Color vec@(V4 r g b _)) = HSVColor h s v af
    where -- we convert from an byte to a float between 1 and zero for intensity
          fromWord :: Word8 -> Float
          fromWord w = fromIntegral w / 255.0
          -- get max and min components in our color
          maxcomp = max r $ max b g
          mincomp = min r $ min b g
          -- get red, green, and blue intensities
          (V4 rf gf bf af) = fromWord <$> vec
          delta   = fromWord maxcomp - fromWord mincomp
          getHue 0.0 _ = 0.0
          getHue _ m | m == r    = (gf - bf) / delta
                     | m == g    = (bf - rf) / delta + 2.0
                     | otherwise = (rf - gf) / delta + 4.0
          -- we want to restrict the hue to within 0 and 360
          h = restrictHue . (*60.0) . getHue delta $ maxcomp
          s = if maxcomp == 0 then 0.0 else 1.0 - (fromIntegral mincomp / fromIntegral maxcomp)
          v = fromWord maxcomp

-- Converts an HSV color into an RGB color
fromHSV :: HSVColor -> Color
fromHSV hsv = let fromFloat :: Float -> Word8
                  fromFloat = floor . (*255.0)
                  alph = fromFloat . hsvAlpha $ hsv
                  hi   = (`mod`6) . floor . (/60.0) . hue $ hsv
                  f    = (hue hsv / 60.0) - (fromInteger . floor) (hue hsv / 60.0)
                  v = fromFloat . (*(value hsv)) . (*255.0) $ 1.0
                  p = fromFloat . (*(value hsv)) . (*255.0) $ 1.0 - saturation hsv
                  q = fromFloat . (*(value hsv)) . (*255.0) $ 1.0 - f * saturation hsv
                  t = fromFloat . (*(value hsv)) . (*255.0) $ 1.0 - (1.0 - f) * saturation hsv in
              case hi of
                  0 -> mkColor v t p alph
                  1 -> mkColor q v p alph
                  2 -> mkColor p v t alph
                  3 -> mkColor p q v alph
                  4 -> mkColor t p v alph
                  _ -> mkColor v p q alph
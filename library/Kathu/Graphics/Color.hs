{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}

module Kathu.Graphics.Color
    ( Color(..)
    , HSVColor(..)
    -- RGB
    , mkColor
    , desaturate
    , desaturateBy
    , blendColor
    , invertRGB
    , nearestColor
    , brightestColor
    , darkestColor
    -- HSV
    , getHSVDifference
    , applyHSVDifference
    , shiftHue
    , shiftHueTowards
    , shiftHueTowardsAbs
    , invertHue
    , blendHSV
    -- Conversion
    , fromHSVFunction
    , fromRGBFunction
    , fromHSV
    , fromRGB
    -- Common colors
    , black
    , white
    , red
    , green
    , yellow
    , blue
    , cyan
    , purple
    )
    where

import           Data.Aeson
import           Data.Aeson.Types      (typeMismatch)
import           Data.Fixed            (mod')
import qualified Data.Foldable         as F
import           Data.Function         (on)
import           Data.List             (foldl')
import qualified Data.Text             as T
import           Data.Word
import           Foreign.Storable
import           GHC.Generics
import           Linear.V4             (V4(..))
import           Numeric               (readHex)

import           Kathu.Util.Collection (padShowHex)
import           Kathu.Util.Flow       (readElseFail)
import           Kathu.Util.Numeric    (closestToZero)

-----------
-- Color --
-----------

newtype Color = Color {unColor :: V4 Word8} deriving (Eq, Generic, Storable)

instance Show Color where
    show (Color (V4 r g b a)) = ('#':) . padShowHex 2 r . padShowHex 2 g . padShowHex 2 b . padShowHex 2 a $ ""
instance Read Color where
    readsPrec _ = parseWithAlpha
        where pair a b = (\case {[] -> Nothing; ((x, _):_) -> Just x}) . readHex $ [a, b]
              orNext f _ Nothing          = f
              orNext _ remainder (Just c) = [(c, remainder)]
              -- we try to parse including an alpha value, or else we'll try without
              parseWithAlpha s@('#':r1:r2:g1:g2:b1:b2:a1:a2:remainder) =
                  orNext (parseNoAlpha s) remainder
                  $ mkColor <$> pair r1 r2 <*> pair g1 g2 <*> pair b1 b2 <*> pair a1 a2
              parseWithAlpha s = parseNoAlpha s
              -- we'll try without an alpha, otherwise we'll fail
              parseNoAlpha   ('#':r1:r2:g1:g2:b1:b2:remainder) =
                  orNext [] remainder
                  $ mkColor <$> pair r1 r2 <*> pair g1 g2 <*> pair b1 b2 <*> Just 255
              parseNoAlpha _   = []

instance ToJSON Color where
    toJSON = toJSON . show
instance FromJSON Color where
    parseJSON (String s) = readElseFail failMsg . T.unpack $ s
        where failMsg = concat ["Couldn't parse String \"", show s, "\" into Color"]
    parseJSON e          = typeMismatch "Color" e

mkColor :: Word8 -> Word8 -> Word8 -> Word8 -> Color
mkColor r g b a = Color $ V4 r g b a

---------------
-- HSV Color --
---------------

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
          luminosity = floor $ 0.21 * (fromIntegral r :: Double) + 0.72 * fromIntegral g + 0.07 * fromIntegral b

desaturateBy :: RealFrac f => f -> Color -> Color
desaturateBy percent color = blendColor percent color (desaturate color)

-- ratio is ratio of 2nd color to 1st; so 1.0 is only 2nd color, etc.
blendColor :: RealFrac f => f -> Color -> Color -> Color
blendColor ratio (Color (V4 r1 g1 b1 a1)) (Color (V4 r2 g2 b2 a2)) = Color $ V4 (blEach r1 r2) (blEach g1 g2) (blEach b1 b2) (blEach a1 a2) 
    where blEach x y = floor $ (1.0 - ratio) * fromIntegral x + ratio * fromIntegral y

invertRGB :: Color -> Color
invertRGB (Color (V4 r g b a)) = Color $ V4 (255 - r) (255 - g) (255 - b) a

-- | Yields the most-visually similar color to the given color within the color collection; uses Euclidean method
-- | If the color set is empty, then the given color is returned as is
nearestColor :: (Foldable t, Functor t) => t Color -> Color -> Color
nearestColor colors color = fst . foldl' minWeight (color, 1/0) . fmap (weigh color) $ colors
    where minWeight (ac, aw) (cc, cw) = if aw < cw then (ac, aw) else (cc, cw)
          weigh :: Color -> Color -> (Color, Double)
          weigh (Color (V4 xr xb xg xa)) c@(Color (V4 yr yb yg ya)) = (c, weight)
              where weight   = (2 + rAvg / 256) * dsqr xr yr + 4 * dsqr xg yg + (2 + (255 - rAvg) / 256) * dsqr xb yb + 6 * dsqr xa ya
                    rAvg     = (fromIntegral xr + fromIntegral yr) / 2
                    dsqr a b = (fromIntegral a - fromIntegral b) ** 2

brightestColor :: (Foldable t, Functor t) => t Color -> Maybe Color
brightestColor = colorByBrightness F.maximumBy

darkestColor :: (Foldable t, Functor t) => t Color -> Maybe Color
darkestColor = colorByBrightness F.minimumBy

colorByBrightness :: (Foldable t, Functor t) => (forall a. (a -> a -> Ordering) -> t a -> a) -> t Color -> Maybe Color
colorByBrightness chooseColor = getMin . fmap brightness
    where getMin f | F.null f  = Nothing
                   | otherwise = Just . fst . chooseColor (compare `on` snd) $ f
          weight f a = fromIntegral a / 255 * f
          -- gets the brightness weighting by human perception
          brightness :: Color -> (Color, Double)
          brightness c@(Color (V4 r g b a)) = (c, weight 0.30 r + weight 0.59 g + weight 0.11 b + weight 0.25 a)

--------------------
-- Manipulate HSV --
--------------------

-- Hue should always be between 0 and 360
restrictHue :: RealFrac f => f -> f
restrictHue h | h < 0   = restrictHue (h + 360)
              | h > 360 = restrictHue (h - 360)
              | otherwise = h

shiftTowardsAngle :: RealFrac f => f -> f -> f -> f
shiftTowardsAngle angle shift h = restrictHue $ h + (if angle < h then distIfH else distIfL)
    where distIfH = if (h - angle) > (angle - h + 360) then shift else -shift
          distIfL = if (angle - h) > (h - angle + 360) then -shift else shift

shiftTowardsAngleByPercent :: RealFrac f => f -> f -> f -> f
shiftTowardsAngleByPercent angle p h = restrictHue $ h + p * dist
    where dist       = closestToZero distTo distAround
          distTo     = if angle < h then negate (h - angle) else angle - h
          distAround = if angle < h then angle - h + 360 else negate (h - angle + 360)

getHSVDifference :: HSVColor -> HSVColor -> HSVColor
getHSVDifference primary col = HSVColor (difVal hue) (difVal saturation) (difVal value) (difVal hsvAlpha)
    where difVal getter = getter primary - getter col

applyHSVDifference :: HSVColor -> HSVColor -> HSVColor
applyHSVDifference template col = HSVColor {hue = (restrictHue . addVal) hue, saturation = addVal saturation, value = addVal value, hsvAlpha = addVal hsvAlpha}
    where addVal getter = getter col + getter template

shiftHue :: Float -> HSVColor -> HSVColor
shiftHue angle (HSVColor h s v a) = HSVColor h' s v a
    where h' = (h + angle) `mod'` 360.0

shiftHueTowards :: Float -> Float -> HSVColor -> HSVColor
shiftHueTowards angle p (HSVColor h s v a) = HSVColor h' s v a
    where h' = shiftTowardsAngleByPercent angle p h

shiftHueTowardsAbs :: Float -> Float -> HSVColor -> HSVColor
shiftHueTowardsAbs angle shift (HSVColor h s v a) = HSVColor h' s v a
    where h' = shiftTowardsAngle angle shift h

invertHue :: HSVColor -> HSVColor
invertHue = shiftHue 180 -- shift to exactly across color wheel

-- ratio is ratio of 2nd color to 1st; so 1.0 is only 2nd color, etc.
blendHSV :: Float -> HSVColor -> HSVColor -> HSVColor
blendHSV ratio (HSVColor h1 s1 v1 a1) (HSVColor h2 s2 v2 a2) = HSVColor h' (blEach s1 s2) (blEach v1 v2) (blEach a1 a2) 
    where blEach x y = (1 - ratio) * x + ratio * y
          h' = shiftTowardsAngleByPercent h1 ratio h2

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
fromHSV hsv =
    let fromFloat :: Float -> Word8
        fromFloat = floor . (*255.0)
        alph = fromFloat . hsvAlpha $ hsv
        hi   = ((`mod`6) . floor . (/60.0) . hue $ hsv) :: Int
        f    = (hue hsv / 60.0) - (fromInteger . floor) (hue hsv / 60.0)
        v = fromFloat . (* value hsv) $ 1.0
        p = fromFloat . (* value hsv) $ 1.0 - saturation hsv
        q = fromFloat . (* value hsv) $ 1.0 - f * saturation hsv
        t = fromFloat . (* value hsv) $ 1.0 - (1.0 - f) * saturation hsv
    in case hi of
        0 -> mkColor v t p alph
        1 -> mkColor q v p alph
        2 -> mkColor p v t alph
        3 -> mkColor p q v alph
        4 -> mkColor t p v alph
        _ -> mkColor v p q alph

-------------------
-- Common Colors --
-------------------

black :: Color
black = mkColor 0 0 0 255

white :: Color
white = mkColor 255 255 255 255

red :: Color
red = mkColor 255 0 0 255

green :: Color
green = mkColor 0 255 0 255

blue :: Color
blue = mkColor 0 0 255 255

yellow :: Color
yellow = mkColor 255 255 0 255

cyan :: Color
cyan = mkColor 0 255 255 255

purple :: Color
purple = mkColor 255 0 255 255
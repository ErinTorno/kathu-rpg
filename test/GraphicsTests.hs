module GraphicsTests (graphicsTests) where

import Test.HUnit

import Kathu.Graphics.Color

graphicsTests :: Test
graphicsTests = TestList
    [ TestLabel "Convert Color 1" colConv1
    , TestLabel "Convert RGB HSV 1" convRGBHSV1
    , TestLabel "Convert RGB HSV 2" convRGBHSV2
    , TestLabel "Shift Hue 1" hueShift1
    , TestLabel "Shift Hue Towards 1" hueShiftTowards1
    , TestLabel "Shift Hue Towards 2" hueShiftTowards2
    , TestLabel "Nearest Color Simple 1" nearestColor1
    , TestLabel "Nearest Color Empty Set" nearestColorEmptySet
    , TestLabel "Nearest Color Red-Green Bias" nearestColorRedGreenBias
    , TestLabel "Nearest Color to Alpha" nearestColorToAlpha
    , TestLabel "Nearest Color to Opaqua" nearestColorToOpaque
    ]

-----------
-- Color --
-----------

convertColor :: String -> String
convertColor c = show col
    where col :: Color
          col = read c

colConv1 :: Test
colConv1 = TestCase $ assertEqual "Color can be converted from String and back" col (convertColor col)
    where col = "#112233ff"

convRGBHSV1 :: Test
convRGBHSV1 = TestCase $ assertEqual "Color RGB to HSV to RGB conversion" col (fromHSV . fromRGB $ col)
    where col = read "#f8ffb1"

convRGBHSV2 :: Test
convRGBHSV2 = TestCase $ assertEqual "Color HSV to RGB conversion" (read "#f7ffb0") (fromHSV $ col)
    where col = (HSVColor 65.4 0.306 1.0 1.0)

hueShift1 :: Test
hueShift1 = TestCase $ assertEqual "Color shift by 30 degrees" (read "#f8ffb1") (fromHSVFunction (shiftHue 30) . read $ "#ffdfb1")

hueShiftTowards1 :: Test
hueShiftTowards1 = TestCase $ assertEqual "Color shift towards hue 35 by 10" (HSVColor 100 1 1 1) (shiftHueTowardsAbs 35 10 $ HSVColor 110 1 1 1)

hueShiftTowards2 :: Test
hueShiftTowards2 = TestCase $ assertEqual "Color shift towards hue 30 by 30" (HSVColor 10 1 1 1) (shiftHueTowardsAbs 30 30 $ HSVColor 340 1 1 1)

-- nearestColor

nearestColor1 :: Test
nearestColor1 = TestCase $ assertEqual "Color was matched from set" (colorSet !! 0) (nearestColor colorSet (read "#aa6610"))
    where colorSet = read <$> ["#ff0000", "#00ff00", "#0000ff"]

nearestColorEmptySet :: Test
nearestColorEmptySet = TestCase $ assertEqual "Empty set, so same color is returned" color (nearestColor [] color)
    where color = read "#ffffff"

nearestColorRedGreenBias :: Test
nearestColorRedGreenBias = TestCase $ assertEqual "When equally distant from red and green, red is favored" (colorSet !! 0) (nearestColor colorSet (read "#444400"))
    where colorSet = read <$> ["#ff0000", "#00ff00", "#0000ff"]

nearestColorToAlpha :: Test
nearestColorToAlpha = TestCase $ assertEqual "More transparent color chosen" (colorSet !! 1) (nearestColor colorSet (read "#bb000022"))
    where colorSet = read <$> ["#ff0000ff", "#66000011"]

nearestColorToOpaque :: Test
nearestColorToOpaque = TestCase $ assertEqual label (colorSet !! 0) (nearestColor colorSet (read "#ff000066"))
    where label = "More transparent color is still farther away that opaque, so opaque chosen"
          colorSet = read <$> ["#ff0000ff", "#66000011"]
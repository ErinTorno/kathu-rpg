{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Kathu.Entity.Action where

import Apecs (Component, Map, Storage)
import Control.Lens
import Linear.V2 (V2(..))

import Kathu.Util.Timing

getDiagonal :: Fractional a => a -> a
getDiagonal v = v * 0.707106781187 -- a constant so that we don't need to call the trigonometric functions each time

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq)

data ActionSet = ActionSet
    { _controller      :: ActionSet -> ActionSet
    , _moving          :: !(Maybe Direction)
    , _lastMoving      :: !(Maybe Direction)
    , _facingDirection :: !Direction -- so even if waiting, we still know which direction to draw
    , _isFocused       :: !Bool
    , _usingPrimary    :: !(Maybe Double) -- angle
    , _usingSecondary  :: !(Maybe Double) -- angle; should be changed to TimeStamped to allow for charging
    }
makeLenses ''ActionSet

instance Component ActionSet where type Storage ActionSet = Map ActionSet

newDirection :: ActionSet -> Bool
newDirection aset = view moving aset /= view lastMoving aset

emptyActionSet :: ActionSet
emptyActionSet = ActionSet id Nothing Nothing South False Nothing Nothing

-- unlike NPCs, the player gives input that is later combined
data ActionPressed = ActionPressed
    { _moveNorth    :: !(TimeStamped Bool)
    , _moveEast     :: !(TimeStamped Bool)
    , _moveSouth    :: !(TimeStamped Bool)
    , _moveWest     :: !(TimeStamped Bool)
    , _useFocus     :: !(TimeStamped Bool)
    , _usePrimary   :: !(TimeStamped Bool)
    , _useSecondary :: !(TimeStamped Bool)
    } deriving (Show, Eq)
makeLenses ''ActionPressed

emptyActionPressed :: ActionPressed
emptyActionPressed = ActionPressed noPress noPress noPress noPress noPress noPress noPress
    where noPress = TimeStamped False 0

-- (\n -> if n == Nothing then n else error . show $ n)
getDirection :: ActionPressed -> Maybe Direction
getDirection ap = indexToDirection $ (nsComp + ewComp :: Int) -- positive x is east, positive y is north
    where nsComp = comp iNorth iSouth (ap ^. moveNorth) (ap ^. moveSouth)
          ewComp = comp iEast  iWest  (ap ^. moveEast)  (ap ^. moveWest)
          comp ia ib a b | a' && b'       = if a > b then ia else if b > a then ib else iNone
                         | a'             = ia
                         | b'             = ib
                         | otherwise      = iNone
              where (a', b') = (timedVal a, timedVal b)

-- | When a character with a moving speed starts trying to move with a given movement speed, at what rate do we accelerate them?
movingAcceleration :: Fractional a => a -> a -> a
movingAcceleration mass s = 54 * mass * s * 0.5

-- positive x is east, positive y is south, positive z is upward
getMoveVector :: (Fractional a, Ord a) => V2 a -> a -> a -> Maybe Direction -> V2 a
getMoveVector _ _ _ Nothing           = V2 0 0 
getMoveVector (V2 !x !y) !mass !s (Just dir) =
    let diagS     = getDiagonal s
        acc       = movingAcceleration mass s
        diagAcc   = getDiagonal acc
        diag !x' !y' !ax !ay -- when we are moving diagonally, we need to check both the x and y components
            | x' > diagS && y' > diagS = V2 0 0
            | x' > diagS               = V2 0 ay
            | y' > diagS               = V2 ax 0
            | otherwise                = V2 ax ay
     in case dir of
        North     -> if y < (-s) then V2 0 0 else V2 0 (-acc)
        Northeast -> diag x (-y) diagAcc (-diagAcc)
        East      -> if x > s then V2 0 0 else V2 acc 0
        Southeast -> diag x y diagAcc diagAcc
        South     -> if y > s then V2 0 0 else V2 0 acc
        Southwest -> diag (-x) y (-diagAcc) diagAcc
        West      -> if x < (-s) then V2 0 0 else V2 (-acc) 0
        Northwest -> diag (-x) (-y) (-diagAcc) (-diagAcc)
 

-- Direction Components

isNorth :: Direction -> Bool
isNorth d | d == North || d == Northeast || d == Northwest = True
          | otherwise = False

isSouth :: Direction -> Bool
isSouth d | d == South || d == Southeast || d == Southwest = True
          | otherwise = False

isWest :: Direction -> Bool
isWest d | d == West || d == Northwest || d == Southwest = True
         | otherwise = False

isEast :: Direction -> Bool
isEast d | d == East || d == Northeast || d == Southeast = True
         | otherwise = False

-- Directions and Int conversions
-- can be added together and then converted back

iNorth, iNortheast, iEast, iSoutheast, iSouth, iSouthwest, iWest, iNorthwest, iNone :: Integral a => a
iNorth     = 1
iNortheast = 5
iEast      = 4
iSoutheast = 3
iSouth     = (-1)
iSouthwest = (-5)
iWest      = (-4)
iNorthwest = (-3)
iNone      = 0

indexToDirection :: (Eq a, Integral a) => a -> Maybe Direction
indexToDirection 1    = Just North
indexToDirection 5    = Just Northeast
indexToDirection 4    = Just East
indexToDirection 3    = Just Southeast
indexToDirection (-1) = Just South
indexToDirection (-5) = Just Southwest
indexToDirection (-4) = Just West
indexToDirection (-3) = Just Northwest
indexToDirection _    = Nothing

-- a temporary solution until we have non absolute animation atlases
dirToAnimIndex :: Integral a => Direction -> a
dirToAnimIndex North     = 0
dirToAnimIndex Northeast = 1
dirToAnimIndex East      = 2
dirToAnimIndex Southeast = 3
dirToAnimIndex South     = 4
dirToAnimIndex Southwest = 5
dirToAnimIndex West      = 6
dirToAnimIndex Northwest = 7
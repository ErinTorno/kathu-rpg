{-# LANGUAGE TemplateHaskell #-}

module Kathu.Entity.Action where

import Control.Lens
import Data.Bool
import Kathu.Entity.Timing
import Linear.V3 (V3(..))

getDiagonalSpeed :: Float -> Float
getDiagonalSpeed v = v * 0.707106781187 -- a constant so that we don't need to call the trigonometric functions each time

type Angle = Float

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq)

data ActionSet = ActionSet
    { _controller :: ActionSet -> ActionSet
    , _moving :: Maybe Direction
    , _lastMoving :: Maybe Direction
    , _facingDirection :: Direction -- so even if waiting, we still know which direction to draw
    , _usingPrimary :: Maybe Angle
    , _usingSecondary :: Maybe Angle
    }
makeLenses ''ActionSet

newDirection aset = view moving aset /= view lastMoving aset

emptyActionSet :: ActionSet
emptyActionSet = ActionSet id Nothing Nothing South Nothing Nothing

-- unlike NPCs, the player gives input that is later combined
data ActionPressed = ActionPressed
    { _moveNorth :: TimeStamped Bool
    , _moveEast :: TimeStamped Bool
    , _moveSouth :: TimeStamped Bool
    , _moveWest :: TimeStamped Bool
    , _usePrimary :: TimeStamped Bool
    , _useSecondary :: TimeStamped Bool
    } deriving (Show, Eq)
makeLenses ''ActionPressed

emptyActionPressed = ActionPressed noPress noPress noPress noPress noPress noPress
    where noPress = TimeStamped False 0

-- (\n -> if n == Nothing then n else error . show $ n)
getDirection :: ActionPressed -> Maybe Direction
getDirection ap = intToDir $ nsComp + ewComp -- positive x is east, positive y is north
    where nsComp = comp iNorth iSouth (ap ^. moveNorth) (ap ^. moveSouth)
          ewComp = comp iEast  iWest  (ap ^. moveEast)  (ap ^. moveWest)
          comp ia ib a b | not (a' || b') = iNone
                         | a' && b'       = if a > b then ia else if b > a then ib else iNone
                         | a'             = ia
                         | b'             = ib
              where (a', b') = (timedVal a, timedVal b)

-- positive x is east, positive y is south, positive z is upward
getMoveVector :: Float -> Direction -> V3 Float
getMoveVector s d = let diagS = getDiagonalSpeed s
                        flatV :: Float -> Float -> V3 Float
                        flatV x y = V3 x y 0.0
                    in case d of
                        North     -> flatV 0.0 (-s)
                        Northeast -> flatV diagS (-diagS)
                        East      -> flatV s 0.0
                        Southeast -> flatV diagS diagS
                        South     -> flatV 0.0 s
                        Southwest -> flatV (-diagS) diagS
                        West      -> flatV (-s) 0.0
                        Northwest -> flatV (-diagS) (-diagS)

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

iNorth     = 1
iNortheast = 5
iEast      = 4
iSoutheast = 3
iSouth     = (-1)
iSouthwest = (-5)
iWest      = (-4)
iNorthwest = (-3)
iNone      = 0

intToDir :: Int -> Maybe Direction
intToDir 1    = Just North
intToDir 5    = Just Northeast
intToDir 4    = Just East
intToDir 3    = Just Southeast
intToDir (-1) = Just South
intToDir (-5) = Just Southwest
intToDir (-4) = Just West
intToDir (-3) = Just Northwest
intToDir _    = Nothing

-- a temporary solution until we have non absolute animation atlases
dirToAnimIndex :: Direction -> Int
dirToAnimIndex North     = 0
dirToAnimIndex Northeast = 1
dirToAnimIndex East      = 2
dirToAnimIndex Southeast = 3
dirToAnimIndex South     = 4
dirToAnimIndex Southwest = 5
dirToAnimIndex West      = 6
dirToAnimIndex Northwest = 7
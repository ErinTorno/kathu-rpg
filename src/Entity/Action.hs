module Entity.Action where

import Data.Bool
import qualified SDL

import Entity.Timing

getDiagonalSpeed :: Float -> Float
getDiagonalSpeed v = v * 0.707106781187 -- a constant so that we don't need to call the trigonometric functions each time

type Angle = Float

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq)

data ActionSet = ActionSet
    { controller :: ActionSet -> ActionSet
    , moving :: Maybe Direction
    , lastMoving :: Maybe Direction
    , usingPrimary :: Maybe Angle
    , usingSecondary :: Maybe Angle
    }

newDirection aset = moving aset /= lastMoving aset

emptyActionSet :: ActionSet
emptyActionSet = ActionSet id Nothing Nothing Nothing Nothing

-- unlike NPCs, the player gives input that is later combined
data ActionPressed = ActionPressed
    { moveNorth :: TimeStamped Bool
    , moveEast :: TimeStamped Bool
    , moveSouth :: TimeStamped Bool
    , moveWest :: TimeStamped Bool
    , usePrimary :: TimeStamped Bool
    , useSecondary :: TimeStamped Bool
    } deriving (Show, Eq)

emptyActionPressed = ActionPressed noPress noPress noPress noPress noPress noPress
    where noPress = TimeStamped False 0

-- (\n -> if n == Nothing then n else error . show $ n)
getDirection :: ActionPressed -> Maybe Direction
getDirection (ActionPressed no ea so we _ _) = intToDir $ ewComp (timedVal ea) (timedVal we) + snComp (timedVal so) (timedVal no) -- positive x is east, positive y is north
    where ewComp e w | not (e || w) = iNone --both are false
                     | e && w       = if ea > we then iEast else if we > ea then iWest else iNone -- error (show e ++ " " ++ show w)
                     | e && not w   = iEast
                     | w && not e   = iWest
          snComp s n | not (s || n) = iNone
                     | s && n       = if so > no then iSouth else if no > so then iNorth else iNone
                     | s && not n   = iSouth
                     | n && not s   = iNorth

-- positive x is east, positive y is south, positive z is upward
getMoveVector :: Float -> Direction -> SDL.V3 Float
getMoveVector s d = let diagS = getDiagonalSpeed s
                        flatV :: Float -> Float -> SDL.V3 Float
                        flatV x y = SDL.V3 x y 0.0
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
iSouth     = -1
iSouthwest = -5
iWest      = -4
iNorthwest = -3
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

{-
dirToInt :: Direction -> Int
dirToInt North     = 1
dirToInt Northeast = 5
dirToInt East      = 4
dirToInt Southeast = 3
dirToInt South     = -1
dirToInt Southwest = -5
dirToInt West      = -4
dirToInt Northwest = -3
-}
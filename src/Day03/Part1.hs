module Day03.Part1 where

{-
 - You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
 -
 - Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
 -
 - 17  16  15  14  13
 - 18   5   4   3  12
 - 19   6   1   2  11
 - 20   7   8   9  10
 - 21  22  23---> ...
 - While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
 -
 - For example:
 -
 - Data from square 1 is carried 0 steps, since it's at the access port.
 - Data from square 12 is carried 3 steps, such as: down, left, left.
 - Data from square 23 is carried only 2 steps: up twice.
 - Data from square 1024 must be carried 31 steps.
 - How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
 -}
{-
 - 4 3 2
 - 5 0 1
 - 6 7 8
 -
 - y
 - ^
 - |
 - o -> x
 -
 - 0 0, 0
 - 1 1, 0
 - 2 1, 1
 - 3 0, 1
 - 4 -1, 1
 - 5 -1, 0
 - 6 -1, -1
 - 7 0, -1
 - 8 1, -1
 - 9 2, -1
 -}
import Prelude hiding (Left, Right)

data Direction
  = Right
  | Up
  | Left
  | Down
  deriving (Show, Enum)

nextDir :: Direction -> Direction
nextDir Down = Right
nextDir x = succ x

data Point a =
  Point a
        a
  deriving (Show, Eq)

type SpiralContext a = (Point a, a, Direction)

nextSpiralPoint :: Integral a => SpiralContext a -> SpiralContext a
nextSpiralPoint (point, size, dir) =
  case dir of
    Right ->
      if moveForwardStillInGrid
        then continueForward (point, size, dir)
        else changeDirection . continueForward . increaseGridSize $
             (point, size, dir)
    _ ->
      if moveForwardStillInGrid
        then continueForward (point, size, dir)
        else continueForward . changeDirection $ (point, size, dir)
  where
    moveForwardStillInGrid = inGrid size . addDir dir $ point
    -- Grid size always increases by 2 when going out of the current perimeter by 1 step
    increaseGridSize (point, size, dir) = (point, size + 2, dir)
    continueForward (point, size, dir) = ((addDir dir point), size, dir)
    changeDirection (point, size, dir) = (point, size, nextDir dir)

{- 1 - 0, 0 -}
{- 3 - -1, 1 -}
{- 5 - -2, 2 -}
{- 7 - -3, 3 -}
-- Size ranges from 1, 3, 5
inGrid :: Integral a => a -> Point a -> Bool
inGrid size (Point x y)
  | x > delta || x < -delta = False
  | y > delta || y < -delta = False
  | otherwise = True
  where
    delta = (size - 1) `div` 2

addDir :: Num a => Direction -> Point a -> Point a
addDir dir (Point x y) =
  case dir of
    Right -> Point (x + 1) y
    Up -> Point x (y + 1)
    Left -> Point (x - 1) y
    Down -> Point x (y - 1)

foldFn :: Integral a => a -> [SpiralContext a] -> [SpiralContext a]
foldFn i acc@(prev:_) = nextSpiralPoint prev : acc

spiralIndices :: Integral a => a -> [SpiralContext a]
spiralIndices i = foldr foldFn [(Point 0 0, 1, Right)] [1 .. i - 1]

solution :: Integral a => a -> a
solution i = abs x + abs y
  where
    extract (point, _, _) = point
    Point x y = extract . head . spiralIndices $ i
{- spiralDebug :: Integral a => a -> [(a, (a, a))] -}
{- spiralDebug i = -}
{-   map (\(index, (point, _, _)) -> (index, point)) $ -}
{-   zip [0 ..] $ spiralIndices i -}

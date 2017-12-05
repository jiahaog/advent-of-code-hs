module Day03.Part1 where

import qualified Data.Map as Map

{-
 - As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
 -
 - So, the first few squares' values are chosen as follows:
 -
 - Square 1 starts with the value 1.
 - Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
 - Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
 - Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
 - Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
 - Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:
 -
 - 147  142  133  122   59
 - 304    5    4    2   57
 - 330   10    1    1   54
 - 351   11   23   25   26
 - 362  747  806--->   ...
 - What is the first value written that is larger than your puzzle input?
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
directions :: Num a => [(a, a)]
directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

result :: Integral a => (a, a) -> a -> Int -> ((a, a), a, Int)
result prev currentSize currentDir =
  if (currentDir `mod` 4) == 0
    then if not (inBoard currentSize sameDirNext)
           then (sameDirNext, currentSize + 2, currentDir + 1)
           else (sameDirNext, currentSize, currentDir)
    else if (inBoard currentSize sameDirNext)
           then (sameDirNext, currentSize, currentDir)
           else (addDir (currentDir + 1) prev, currentSize, currentDir + 1)
  where
    sameDirNext = addDir currentDir prev

{- 1 - 0, 0 -}
{- 3 - -1, 1 -}
{- 5 - -2, 2 -}
{- 7 - -3, 3 -}
-- Size ranges from 1, 3, 5
inBoard :: Integral a => a -> (a, a) -> Bool
inBoard size (x, y)
  | x > delta || x < -delta = False
  | y > delta || y < -delta = False
  | otherwise = True
  where
    delta = (size - 1) `div` 2

addDir :: Num a => Int -> (a, a) -> (a, a)
addDir dir (x, y) = (x + toAddX, y + toAddY)
  where
    dirIndex = dir `mod` (length directions)
    (toAddX, toAddY) = (directions !! dirIndex)

foldFn :: Integral a => a -> [((a, a), a, Int)] -> [((a, a), a, Int)]
foldFn i acc@((prev, currentSize, currentDir):_) =
  (result prev currentSize currentDir) : acc

spiralIndicesWorking :: Integral a => a -> [((a, a), a, Int)]
spiralIndicesWorking i = foldr foldFn [((0, 0), 1, 0)] [1 .. i - 1]
{- something :: Integral a => a -> Map.Map a  -}
{- spiralIndices :: Integral a => a -> [(a, (a, a))] -}
{- spiralIndices i = -}
{-   map (\(index, (point, _, _)) -> (index, point)) $ -}
{-   zip [0 ..] $ spiralIndicesWorking i -}
{- solution :: Integral a => a -> a -}
{- solution i = abs x + abs y -}
{-   where -}
{-     extract (point, _, _) = point -}
{-     (x, y) = extract . head . spiralIndicesWorking $ i -}

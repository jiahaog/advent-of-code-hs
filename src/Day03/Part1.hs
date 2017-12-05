module Day03.Part1 where
 -- Search -- Descend by smallest
 --

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

spiralIndices :: Integral a => a -> [(a, (a, a))]
spiralIndices i =
  map (\(index, (point, _, _)) -> (index, point)) $
  zip [0 ..] $ spiralIndicesWorking i

solution :: Integral a => a -> a
solution i = abs x + abs y
  where
    extract (point, _, _) = point
    (x, y) = extract . head . spiralIndicesWorking $ i

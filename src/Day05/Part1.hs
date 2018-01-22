module Day05.Part1 where

{-
 - An urgent interrupt arrives from the CPU: it's trapped in a maze of jump instructions, and it would like assistance from any programs with spare cycles to help find the exit.
 -
 - The message includes a list of the offsets for each jump. Jumps are relative: -1 moves to the previous instruction, and 2 skips the next one. Start at the first instruction in the list. The goal is to follow the jumps until one leads outside the list.
 -
 - In addition, these instructions are a little strange; after each jump, the offset of that instruction increases by 1. So, if you come across an offset of 3, you would move three instructions forward, but change it to a 4 for the next time it is encountered.
 -
 - For example, consider the following list of jump offsets:
 -
 - 0
 - 3
 - 0
 - 1
 - -3
 - Positive jumps ("forward") move downward; negative jumps move upward. For legibility in this example, these offset values will be written all on one line, with the current instruction marked in parentheses. The following steps would be taken before an exit is found:
 -
 - (0) 3  0  1  -3  - before we have taken any steps.
 - (1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
 -  2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
 -  2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
 -  2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
 -  2  5  0  1  -2  - jump 4 steps forward, escaping the maze.
 - In this example, the exit is reached in 5 steps.
 -
 - How many steps does it take to reach the exit?
 -}
solution :: [Int] -> Maybe Int
solution x = jumpRecur (x, 0, 0)

jumpRecur :: Current -> Maybe Int
jumpRecur current =
  case jump current of
    Just arg@(steps, i, counter) ->
      if i >= length steps
        then return counter
        else jumpRecur arg
    _ -> Nothing

type Current
   = ( [Int] -- ^ the list of steps
     , Int -- ^ the current position (index)
     , Int -- ^ counter keeping track of the number of jumps so far
      )

jump :: Current -> Maybe Current
jump (steps, i, counter) =
  case safeElem steps i of
    Just x -> return (nextSteps, nextIndex, counter + 1)
      where nextSteps = replaceElem steps i (x + 1)
            nextIndex = x + i
    _ -> Nothing

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

safeElem :: [a] -> Int -> Maybe a
safeElem xs index =
  foldr
    (\(i, x) acc ->
       case acc of
         Just y -> Just y
         _ ->
           if i == index
             then Just x
             else Nothing)
    Nothing $
  zip [0 ..] xs

replaceElem :: [a] -> Int -> a -> [a]
replaceElem xs newI newX =
  foldr
    (\(i, x) acc ->
       (if i == newI
          then newX
          else x) :
       acc)
    []
    (zip [0 ..] xs)

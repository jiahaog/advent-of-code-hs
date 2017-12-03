module Day01.Part1 where

{-
 - The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.
 - For example:
 - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
 - 1111 produces 4 because each digit (all 1) matches the next.
 - 1234 produces 0 because no digit matches the next.
 - 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
 -}
stringToIntArr :: Read a => String -> [a]
stringToIntArr = map $ read . (: [])

group :: [a] -> [[a]]
group [] = []
{- because of the drop 1 we will always end up with a length one list at the end -}
group [x] = []
group x = (take 2 x) : (group . (drop 1)) x

appendFirst :: [a] -> [a]
appendFirst (x:xs) = x : xs ++ [x]

doubleMatch :: Eq a => [a] -> Bool
doubleMatch (x:y:_) = x == y

maybeAdd :: (Eq a, Num a) => a -> [a] -> a
maybeAdd acc pair@(x:_) =
  if doubleMatch pair
    then acc + x
    else acc

reduce :: (Eq a, Num a) => [[a]] -> a
reduce = foldl maybeAdd 0

solution :: (Eq a, Num a, Read a) => String -> a
solution = reduce . group . appendFirst . stringToIntArr

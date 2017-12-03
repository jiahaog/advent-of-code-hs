module Day01.Part2 where

{-
 - Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list. That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward matches it. Fortunately, your list has an even number of elements.
 - For example:
 - 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
 - 1221 produces 0, because every comparison is between a 1 and a 2.
 - 123425 produces 4, because both 2s match each other, but no other digit has a match.
 - 123123 produces 12.
 - 12131415 produces 4.
 - What is the solution to your new captcha?
 -}
stringToIntArr :: Read a => String -> [a]
stringToIntArr = map $ read . (: [])

nextIndex :: [a] -> Int -> Int
nextIndex xs i = (i + halfLen) `mod` len
  where
    len = length xs
    halfLen = len `div` 2

withIndex :: (Enum b, Num b) => [a] -> [(b, a)]
withIndex = zip [0 ..]

buildProcessElem :: (Eq a, Num a) => [a] -> (a -> (Int, a) -> a)
buildProcessElem xs =
  \acc (i, x) ->
    let counterPart = (xs !! (nextIndex xs i))
    in if x == counterPart
         then acc + x
         else acc

reduce :: (Eq a, Num a) => [a] -> a
reduce xs = foldl processElem 0 $ withIndex xs
  where
    processElem = buildProcessElem xs

solution :: (Eq a, Num a, Read a) => String -> a
solution = reduce . stringToIntArr

module Day04.Part2 where

import qualified Data.Map as Map

{-
 - A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password. A passphrase consists of a series of words (lowercase letters) separated by spaces.
 -
 - To ensure security, a valid passphrase must contain no duplicate words.
 -
 - For example:
 -
 - aa bb cc dd ee is valid.
 - aa bb cc dd aa is not valid - the word aa appears more than once.
 - aa bb cc dd aaa is valid - aa and aaa count as different words.
 - The system's full passphrase list is available as your puzzle input. How many passphrases are valid?
 -}
import qualified Data.Set as Set

insertLetter :: (Ord k, Num v) => Map.Map k v -> k -> Map.Map k v
insertLetter map x = Map.insertWithKey insertFunc x 1 map
  where
    insertFunc k newVal oldVal = newVal + oldVal

toLetterMap :: Num v => String -> Map.Map Char v
toLetterMap = foldr foldFn Map.empty
  where
    foldFn x acc = insertLetter acc x

solution :: [String] -> Bool
-- x == y includes the case where x and y are the same letter maps, so we can have a minimum of
-- `length xs` elements in the array assuming that there are zero matches. Hence additional
-- elements in the array are because their letterMaps matches, and we should return false then
solution xs = length [1 | x <- letterMaps, y <- letterMaps, x == y] == length xs
  where
    letterMaps = map toLetterMap xs

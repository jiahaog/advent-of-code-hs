module Day04.Part2 where

import qualified Data.Map as Map

{-
 - For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
 -
 - For example:
 -
 - abcde fghij is a valid passphrase.
 - abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
 - a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
 - iiii oiii ooii oooi oooo is valid.
 - oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
 - Under this new system policy, how many passphrases are valid?
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

import Data.Char

import Data.Char (isSpace)

-- import Day02.Part1 (solution)
import Day02.Part2 (solution)

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

parse :: String -> [[Integer]]
parse = (map $ map read) . preprocess
  where
    preprocess = map words . lines

main = do
  contents <- getContents
  putStr $ show . solution . parse . rstrip $ contents

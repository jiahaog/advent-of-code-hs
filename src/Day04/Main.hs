module Day04.Main where

import Data.Char

import qualified Day04.Part1 as Part1
import qualified Day04.Part2 as Part2

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

main :: String -> String -> IO ()
main partNum inp = do
  putStrLn $ show . length . filterTrue . mapSolution . strippedLines $ inp
  where
    filterTrue = filter id
    solution =
      case partNum of
        "2" -> Part2.solution
        _ -> Part1.solution
    mapSolution = map $ solution . words
    strippedLines = lines . rstrip

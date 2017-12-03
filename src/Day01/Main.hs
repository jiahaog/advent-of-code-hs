module Day01.Main where

import Data.Char

import qualified Day01.Part1 as Part1
import qualified Day01.Part2 as Part2

main :: String -> String -> IO ()
main partNum inp = do
  putStrLn $ show . solution . head . lines $ inp
  where
    solution =
      case partNum of
        "2" -> Part2.solution
        _ -> Part1.solution

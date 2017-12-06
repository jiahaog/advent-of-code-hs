module Main where

import Data.List (lookup)
import qualified Day01.Main as Day01
import qualified Day03.Main as Day03
import qualified Day04.Main as Day04
import System.Environment (getArgs)
import System.Exit (exitFailure)

dispatch :: [(String, String -> String -> IO ())]
dispatch = [("1", Day01.main), ("3", Day03.main), ("4", Day04.main)]

main = do
  args <- getArgs
  putStrLn $ currentTask args
  stdin <- getContents
  case args of
    (command:args) ->
      let action = lookup command dispatch
      in handleAction action (head args) stdin
    _ -> do
      putStr helpText
      exitFailure

handleAction :: Maybe (String -> String -> IO ()) -> String -> String -> IO ()
handleAction m partName stdin =
  case m of
    (Just action) -> action partName stdin
    _ -> do
      putStr helpText
      exitFailure

helpText :: String
helpText =
  "Usage: advent-of-code-exe DAY PART\n\
  \\n\
  \STDIN will be used as the input\n\
  \Example: To run day01 part1:\n\
  \    advent-of-code-exe 01 1\n"

currentTask :: [String] -> String
currentTask (day:part:_) = "Running day " ++ day ++ " part " ++ part

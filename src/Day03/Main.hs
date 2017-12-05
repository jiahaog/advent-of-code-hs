module Day03.Main where

import Data.Char

import Day03.Part1

main :: String -> String -> IO ()
main _ inp = do
  putStrLn $ show . solution . read . head . lines $ inp

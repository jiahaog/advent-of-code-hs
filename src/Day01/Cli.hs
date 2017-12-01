import Data.Char

{- import Day01.Solution (solution) -}
import Day01.Part2 (solution)

main = do
  contents <- getContents
  putStr $ show . solution . head . lines $ contents

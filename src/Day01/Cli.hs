import Data.Char
import Day01.Solution (solution)

main = do
  contents <- getContents
  putStr $ show . solution . head . lines $ contents

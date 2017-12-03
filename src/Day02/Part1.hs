module Day02.Part1 where

arrayDiff :: (Num a, Ord a) => [a] -> a
arrayDiff xs = maximum xs - minimum xs

solution :: (Num a, Ord a) => [[a]] -> a
solution = sum . map arrayDiff

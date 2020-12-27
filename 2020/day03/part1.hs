module Main where

main = interact $ show . length . treesOnSlope 0 1 2 . map cycle . lines

treeAt start shift (i, row) = '#' == row !! (start + shift * i)

treesOnSlope start right down = filter (treeAt start right) . zip [0..] . everyN down

everyN n [] = []
everyN n (x:xs) = x : everyN n (drop (n-1) xs)

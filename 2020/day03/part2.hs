module Main where

main = interact $ show . multiTrees . map cycle . lines

multiTrees trees = treesOnSlope 0 1 1 trees
                 * treesOnSlope 0 3 1 trees
                 * treesOnSlope 0 5 1 trees
                 * treesOnSlope 0 7 1 trees
                 * treesOnSlope 0 1 2 trees

treeAt start shift (i, row) = '#' == row !! (start + shift * i)

treesOnSlope start right down = length . filter (treeAt start right) . zip [0..] . everyN down

everyN n [] = []
everyN n (x:xs) = x : everyN n (drop (n-1) xs)

module Main where

main = interact $ show . foldr (\x y -> if x == '(' then 1 + y else y - 1) 0 . concat . lines

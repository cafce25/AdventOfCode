module Main where

main = interact $ show . foldr (\(pos, x) (p, y) -> if x == '(' then 1 + y else y - 1) 0 . zip [1..] . concat . lines

whenFirstAtFloor floor 

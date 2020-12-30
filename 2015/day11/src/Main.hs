module Main where

import Control.Arrow (first)
import Data.Char (chr, ord)
import Data.List (foldl', unfoldr, intersect, group)
import Data.Tuple (swap)

type Input = String

readAlpha26 :: String -> Int
readAlpha26 = foldl' (\acc e -> acc * 26 + charToInt e) 0
    where charToInt = subtract (ord 'a') . ord

showAlpha26 :: Int -> String
showAlpha26 = reverse . unfoldr (\x' -> case first (chr . (+ ord 'a')) . swap . (`divMod` 26) $ x' of
                         ('a', 0) -> Nothing
                         x -> Just x
                      )

has3Consecutive :: String -> Bool
has3Consecutive = (>= 2) . maximum . (0:) . map length . filter (1 `elem`) . group . differences
    where differences (a:xs@(b:_)) = (ord b - ord a):differences xs
          differences _ = []

doublePair :: String -> Bool
doublePair [] = False
doublePair [_] = False
doublePair (a:b:xs)
  | a == b = singlePair xs
  | otherwise = doublePair (b:xs)

singlePair :: String -> Bool
singlePair [] = False
singlePair [_] = False
singlePair (a:b:xs)
  | a == b = True
  | otherwise = singlePair (b:xs)


noneOf :: String -> String -> Bool
noneOf x = null . intersect x

validPw :: String -> Bool
validPw = and . sequence [has3Consecutive, noneOf "iol", doublePair]

validPws :: [String]
validPws = filter validPw . map showAlpha26 . iterate succ . readAlpha26 $ "cqjxjnds"

part1 :: String
part1 = head validPws

part2 :: String
part2 = validPws !! 1

main :: IO ()
main = print (part1, part2)


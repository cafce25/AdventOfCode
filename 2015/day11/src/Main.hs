module Main where

import Control.Arrow ((&&&), first)
import Data.Char (chr, ord)
import Data.List (foldl', unfoldr, intersect, group)
import Data.Tuple (swap)
import System.Environment (getArgs)

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

part1 :: Input -> String
part1 = head . filter validPw . map showAlpha26 . iterate succ . readAlpha26

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = const "cqjxjnds"

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

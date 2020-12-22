{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List.Split
import System.Environment (getArgs)

type Input = [[Int]]

diff :: [Int] -> Int
diff xs = maximum xs - minimum xs

part1 :: Input -> Int
part1 = sum . map diff  

myQuot :: [Int] -> Int
myQuot xs = div' $ filter (\e -> 2 == length ( filter (\e' -> 0 == e `mod` e' || e' `mod` e == 0) xs)) xs
    where div' ys = maximum ys `div` minimum ys

part2 :: Input -> Int
part2 = sum . map myQuot

prepare :: String -> Input
prepare = map (map read . splitOn "\t") . lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

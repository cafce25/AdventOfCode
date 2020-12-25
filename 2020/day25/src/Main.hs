module Main where

import Control.Arrow ((&&&), second)
import System.Environment (getArgs)

type Input = (Int, Int)

transform :: Int -> Int -> Int -> Int
transform m s l = transform' l 1
    where transform' 0 v = v
          transform' r v = transform' (pred r) (step m s v)

step :: Int -> Int -> Int -> Int
step m s v = (v * s) `mod` m

loops :: Int -> Int -> Int -> Int
loops m s x = loops' 0 1
    where loops' n v
            | v == x = n
            | otherwise = loops' (succ n) (step m s v)

modulus :: Int
modulus = 20201227

part1 :: Input -> Int
part1 = uncurry (transform modulus) . second (loops modulus 7)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = (head &&& head . tail) . map read . lines

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

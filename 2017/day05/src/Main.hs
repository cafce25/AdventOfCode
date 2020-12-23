{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import System.Environment (getArgs)

type Input = [Int]

jump :: Int -> [Int] -> (Int, [Int])
jump pos ls = (el + pos, new)
    where new = b ++ [succ el] ++ tail a
          (b, a) = splitAt pos ls
          el = ls !! pos

steps :: [Int] -> Int
steps = steps' 0 0
    where steps' n pos ls = if pos >= length ls || pos < 0
                               then n
                               else uncurry (steps' (succ n)) $ jump pos ls

part1 :: Input -> Int 
part1 = steps

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

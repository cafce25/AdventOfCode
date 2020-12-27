{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import System.Environment
import Common
import Part1 (part1)
import Part2 (part2)

prepare :: String -> Input
prepare = map read. lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

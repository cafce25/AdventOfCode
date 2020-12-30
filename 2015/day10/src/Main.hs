module Main where

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.List (group)
import System.Environment (getArgs)

type Input = [Int]

part1 :: Input -> Int
part1 = length . concatMap show . (!!40) . iterate lookandsay

lookandsay :: [Int] -> [Int]
lookandsay = map digitToInt . concatMap (uncurry (++) . (show . length &&& show . head)) . group

part2 :: Input -> Int
part2 = length . concatMap show . (!!50) . iterate lookandsay

prepare :: String -> Input
prepare = const $ map digitToInt "1113222113"

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

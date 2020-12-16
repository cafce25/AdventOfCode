module Main where

import Control.Arrow ((&&&))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Lazy as M
import Data.List.Split (splitOn)
import System.Environment (getArgs)

type Input = [Int]

list :: [Int] -> [Int]
list start = start ++ l (length start) (last start) startMap
    where startMap = M.fromList (zip start [0..(pred.pred.length$ start)])
          l :: Int -> Int -> IntMap Int -> [Int]
          l n lastEl past = this: l (succ n) this (M.insert lastEl (pred n) past) 
              where this = (pred n) - M.findWithDefault (pred n) lastEl past
                                                 
part1 :: Input -> Int
part1 = (!! (pred 2020)) . list

part2 :: Input -> Int
part2 = (!! (pred 30000000)) . list

prepare :: String -> Input
prepare = map read . splitOn ","

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

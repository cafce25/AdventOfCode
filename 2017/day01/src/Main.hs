{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type Input = [Int]

solve :: Input -> Int
solve (x:xs@(y:_)) = if x == y
                        then x + solve xs
                        else solve xs
solve _ = 0

part1 :: Input -> Int
part1 = solve . putlast
    where putlast xs@(x:_) = xs ++ [x]
          putlast _ = error "invalid input"

solve' :: Int -> Input -> Int
solve' n xs = if n >= l
                 then 0
                 else (if xs !! n == xs !! ((n + l2) `mod` l)
                         then ((xs!!n) +)
                         else id) $ solve' (succ n) xs
    where l = length xs
          l2 = l `div` 2



part2 :: Input -> Int
part2 = solve' 0

prepare :: String -> Input
prepare = mapMaybe (readMaybe . (:[])) 

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type Input = [Int]

part1 :: Input -> Int
part1 (x:xs@(y:_)) = if x == y
                        then x + part1 xs
                        else part1 xs
part1 _ = 0

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = putlast . mapMaybe (readMaybe . (:[])) 
    where putlast xs@(x:_) = xs ++ [x]
          putlast _ = error "invalid input"

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

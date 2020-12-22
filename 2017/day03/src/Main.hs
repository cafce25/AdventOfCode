{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((&&&))
import System.Environment (getArgs)

type Input = Int

distance :: Int -> Int
distance n = s2 + if m > s2 then pred s - m else m
    where s = if s' `mod` 2 == 0 then succ s' else s'
          s' = ceiling . sqrt @Double $ fromIntegral n
          c = s2 - 1
          s2 = (s - 1) `div` 2
          m = (n + c) `mod` pred s
part1 :: Input -> Int 
part1 = distance                        

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Int
prepare = fst . head . readsPrec 0

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

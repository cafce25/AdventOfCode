{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
import System.Environment (getArgs)

type Input = IntMap Int

jump :: Int -> Input -> (Int, Input)
jump pos ls = (el + pos, new)
    where new = M.insert pos (succ el) ls
          el = ls ! pos

steps :: (Int -> Input -> (Int, Input)) -> Input -> Int
steps jumpf = steps' 0 0
    where steps' n pos ls = if pos >= length ls || pos < 0
                               then n
                               else uncurry (steps' (succ n)) $ jumpf pos ls

part1 :: Input -> Int 
part1 = steps jump

jump2 :: Int -> Input -> (Int, Input)
jump2 pos ls = (el + pos, new)
    where new = M.insert pos (nextEl) ls
          el = ls ! pos
          nextEl = if el >= 3 then pred el else succ el

part2 :: Input -> Int
part2 = steps jump2

prepare :: String -> Input
prepare = M.fromList . zip [0..] . map read . lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

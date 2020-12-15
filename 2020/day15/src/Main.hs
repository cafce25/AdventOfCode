module Main where

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.List.Split
import Data.Sequence (Seq((:|>)), (|>))
import qualified Data.Sequence as S
import System.Environment

import Debug.Trace

type Input = [Int]
                                                 
part1 :: Input -> Int
part1 inp = list (2020 - length inp) (S.fromList inp)
list :: Int -> Seq Int -> Int
list 0 (_:|>p) = p 
list n s@(l :|> p) = if p `elem` l
                     then list (pred n) (s |> (succ $ length $ takeWhile (/= p) ls))
                     else list (pred n) (s |> 0)
    where ls = reverse $ toList l

part2 :: Input -> ()
part2 = const ()

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

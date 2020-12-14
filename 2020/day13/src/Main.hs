{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import System.Environment
import Text.Read (readMaybe)
import Debug.Trace

data Input = Input Int [Maybe Int]

timeToDep :: Int -> Int -> Int
timeToDep e i = i - (e `mod` i)

part1 :: Input -> Int
part1 (Input e ids) = (\x -> timeToDep e x * x)
                    . head
                    . sortBy (\a b -> on compare (timeToDep e) a b)
                    . catMaybes
                    $ ids

findPerfectTime :: [(Int, Int)] -> Int
findPerfectTime ids = findPerfectTime' (-start)
    where findPerfectTime' t = if all (\(idx, bid) -> 0 == ((t + idx) `mod` bid)) ids
            then t
            else findPerfectTime' (t+base)
          (start, base) = head $ sortBy (on (flip compare) snd) $ (traceShowId ids)

part2 :: Input -> Int
part2 (Input _ ids) = findPerfectTime idsWithIdx
    where idsWithIdx = mapMaybe liftTuple . zip [0..] $ ids
          liftTuple (idx, i) = case i of
                Nothing -> Nothing
                Just busId -> Just (idx, busId)

prepare :: String -> Input
prepare input = Input (read estimate) (map readMaybe (splitOn "," ids))
    where ei = lines input
          estimate = ei !! 0
          ids = ei !! 1

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

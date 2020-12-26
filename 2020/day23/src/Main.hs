{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (findIndex)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type Input = (Int, [Int])

step :: Int -> [Int] -> Input
step l (x:xs') = (l, insertAfter dest is xs ++ [x])
    where (is, xs) = splitAt 3 xs'
          dest = findDestination l x xs
step _ _ = error "empty cup list received"

findDestination :: Int -> Int -> [Int] -> Int
findDestination l x xs
  | x' > 0 = if x' `elem` xs then x' else findDestination l x' xs
  | otherwise = findDestination l (succ l) xs
    where x' = pred x

insertAfter :: Int -> [Int] -> [Int] -> [Int]
insertAfter el is xs = h ++ is ++ t
    where mi = findIndex (==el) xs
          i = case mi of
                Just i' -> succ i'
                Nothing -> 0
          (h, t) = splitAt i xs

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n (cycle xs)) xs

part1 :: Input -> String
part1 = tail . head . dropWhile ((/='1') . head) . iterate (rotate 1) . concat . map show . snd . (!!100) . iterate (uncurry step)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = (length &&& id) . mapMaybe (readMaybe . (:[]))

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

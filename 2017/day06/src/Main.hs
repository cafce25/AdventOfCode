{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import System.Environment (getArgs)

type Input = IntMap Int

distribute :: Input -> Input
distribute banks = M.mapWithKey distribute' banks
    where maxEl = maximum banks
          a = M.assocs banks
          maxI = fst . fromJust . find ((maxEl==) . snd) $ a
          (q, r) = maxEl `divMod` l
          l = length banks
          distribute' k v
              | k == maxI = q
              | maxI + r >= k && k > maxI
                  || maxI + r >= l && k <= (maxI + r) `mod` l = v + q + 1
              | otherwise = v + q

part1 :: Input -> Int
part1 = pred . length . takeWhileNoDupe . iterate distribute  

takeWhileNoDupe :: Ord a => [a] -> [a]
takeWhileNoDupe = takeWhileNoDupe' S.empty
    where takeWhileNoDupe' _ [] = [] 
          takeWhileNoDupe' acc (x:xs) = if x `S.member` acc
                                then [x]
                                else x:(takeWhileNoDupe' (S.insert x acc) xs )

part2 :: Input -> Int
part2 = pred . length . (\x -> dropWhile (/= last x) x) . takeWhileNoDupe . iterate distribute

prepare :: String -> Input
prepare = M.fromList . zip [0..] . map read . words

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((&&&))
import System.Environment (getArgs)
import Data.Map.Ordered (OMap,(|>))
import qualified Data.Map.Ordered as M
import qualified Data.Map as Map

type Input = Int
type Square = OMap (Int, Int) Int

coordinates :: Int -> (Int, Int)
coordinates 1 = (0,0)
coordinates n = case (n - (s - 2) * (s - 2) - 1) `div` pred s of
                  0 -> (r, t)
                  1 -> (-t, r)
                  2 -> (-r, -t)
                  _ -> (t, -r)
    where s = if s' `mod` 2 == 0 then succ s' else s'
          s' = ceiling . sqrt @Double $ fromIntegral n
          c = s2 - 1
          s2 = (s - 1) `div` 2
          m = (n + c) `mod` pred s
          r = s2
          t = if m > s2 then m - pred s else m

part1 :: Input -> Int 
part1 = uncurry manDist . coordinates                        
    where manDist a b = abs a + abs b

genValue :: Input -> Int
genValue l = genValue' 1 (M.singleton ((0,0), 1))
    where genValue' i sq = if v > l then v else genValue' (succ i) (sq |> (c,v))
              where c@(x, y) = coordinates i
                    v = sum [Map.findWithDefault 0 (x+dx, y+dy) m| dx <- [-1..1], dy <- [-1..1]]
                    m = M.toMap sq

part2 :: Input -> Int
part2 = genValue

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

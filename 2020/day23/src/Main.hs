{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Char (digitToInt, intToDigit)
import Data.List (sortOn)
import System.Environment (getArgs)

type Input = [Int]
type Label = Int
type Ary s = STUArray s Label Label

step :: Int -> Label -> Ary s -> ST s (Label, Ary s)
step len l ar = do
  x1 <- readArray ar l
  x2 <- readArray ar x1
  x3 <- readArray ar x2
  let rest = x3
  restN <- readArray ar rest
  lN <- readArray ar l
  let dest = getDestination len l [x1, x2, x3]
  destN <- readArray ar dest
  writeArray ar l restN
  writeArray ar dest lN
  writeArray ar rest destN
  next <- readArray ar l
  pure (next, ar)

stepN :: Int -> Int -> Label -> Ary s -> ST s (Label, Ary s)
stepN 0 _ l ar = return (l, ar)
stepN n len l ar = do
  (l', ar') <- step len l ar
  stepN (n-1) len l' ar'

getDestination :: Int -> Label -> [Label] -> Label
getDestination len el next3 = if nextEl `elem` next3 then getDestination len nextEl next3 else nextEl
  where nextEl = 1 + ( len + el - 2) `mod` len

part1 :: Input -> String
part1 = map intToDigit . tail . swapST 100

swapST :: Int -> [Label] -> [Label]
swapST n labels = runST $ do
  ar <- refArrayFromList labels
  (_, ar') <- stepN n (length labels) (head labels) ar
  far <- freeze ar'
  pure $ arrToList far
  

part2 :: Input -> Int
part2 i = product . take 2 . tail . swapST 10000000 . (<> [1 + length i..1000000]) $ i
    
arrToList :: UArray Label Label -> [Label]
arrToList ary = arrToList' 1 ary
  where arrToList' l ar = if ar!l == 1 then [l] else l:arrToList' (ar!l) ar

refArrayFromList :: [Int] -> ST s (Ary s)
refArrayFromList ls = newListArray (1, length ls) lElems
  where lAssocs = zip ls (drop 1 $ cycle ls)
        lElems = map snd . sortOn fst $ lAssocs

prepare :: String -> Input
prepare = map digitToInt . filter (`elem` ['0'..'9'])

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

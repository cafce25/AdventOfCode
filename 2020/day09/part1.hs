{-# LANGUAGE FlexibleContexts #-}
import System.Environment

main = getArgs >>= parse >>= print . firstNotSumPrefix 25 . map read . lines

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs


--firstNotSumPrefix :: Int -> [Integer] -> Integer
firstNotSumPrefix n xs = firstNotSumPrefix' (take n xs) (drop n xs)
    where firstNotSumPrefix' past (x:xs) = if x `notElem` prefixSums
                then (x, prefixSums)
                else firstNotSumPrefix' (drop 1 (past ++ [x])) xs
              where prefixSums = [x+y | x <- past, y <- past, x /= y]

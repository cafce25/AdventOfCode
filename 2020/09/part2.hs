{-# LANGUAGE FlexibleContexts #-}
import System.Environment

main = getArgs >>= parse >>= print . (\x -> (minimum x) + (maximum x)) . seqThatSumsTo 466456641 10 . map read . lines

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs

seqThatSumsTo :: Integer -> Int -> [Integer] -> [Integer]
seqThatSumsTo s n xs = case compare s (sum list) of
    EQ -> list
    LT -> seqThatSumsTo s (n-1) (tail xs)
    GT -> seqThatSumsTo s (n+1) xs
    where list = take n xs

firstNotSumPrefix :: Int -> [Integer] -> Integer
firstNotSumPrefix n xs = firstNotSumPrefix' (take n xs) (drop n xs)
    where firstNotSumPrefix' past (x:xs) = if x `notElem` prefixSums
                then x
                else firstNotSumPrefix' (drop 1 (past ++ [x])) xs
              where prefixSums = [x+y | x <- past, y <- past, x /= y]

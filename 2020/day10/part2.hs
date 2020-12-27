import Data.List
import System.Environment
import System.IO
import Data.List.Split

main = getArgs
    >>= parse
    >>= print . product
    . map ((tribonacci!!) . length) . splitOneOf [3] . diffs
    . (0:) . sort . map read . lines

diffs (x:y:xs) = (y-x): diffs (y:xs)
diffs _ = []

tribonacci = tribonacci' 0 0 1
    where tribonacci' a b c = c:tribonacci' b c (a+b+c)

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs



--import Control.Applicative hiding (optional)
import Data.List
import System.Environment
import System.IO
import Data.List.Split
--import Data.Map as Map hiding (map)

main = getArgs
    >>= parse
    >>= print . product .map (tribonacci!!) . map length . splitOneOf [3] . diffs . (0:) . sort . (map read :: [String] -> [Integer]) . lines

{- brute force -}
{-func xs = func' empty xs
    where
        func' mem (x:y:z:xs) = if member (x,y,z) mem then mem ! (x, y, z)
            else dropX `seq` this
                where dropY = func' mem (x:z:xs)
                      dropX = func' mem (y:z:xs)
                      this = (if z - x  <= 3 then dropY else 0) + dropX
        func' _ _ = 1
-}

diffs (x:y:xs) = (y-x): diffs (y:xs)
diffs _ = []

tribonacci = tribonacci' 0 0 1
    where tribonacci' a b c = c:tribonacci' b c (a+b+c)

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs



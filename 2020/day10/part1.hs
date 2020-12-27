import Control.Applicative hiding (optional)
import Data.List
import System.Environment
import System.IO
import Text.ParserCombinators.ReadP
import Text.Read
import Data.Map as Map hiding (map)

main = getArgs
    >>= parse
    >>= print . func 0 1 . (0:). sort . (map read :: [String] -> [Int]) . lines

func d1 d3 (x:y:xs) = d1 `seq` d3 `seq` case y-x of
    1 -> func (d1+1) d3 (y:xs)
    3 -> func d1 (d3+1) (y:xs)
    _ -> func d1 d3 (y:xs)
func d1 d3 _ = d1*d3


parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs



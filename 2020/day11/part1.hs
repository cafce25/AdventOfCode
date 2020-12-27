import Data.List hiding (insert)
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map, (!))
import System.Environment
import System.IO
import Debug.Trace

newtype Board = Board (Map (Int, Int) Char) deriving (Eq)

nX, nY :: Int
nX = 96
nY = 90

instance Show Board where
    show (Board board) = intercalate "\n" . chunksOf nX . map snd . Map.toList $ board

main = getArgs
    >>= parse
    >>= print
    . length
    . filter (=='#')
    . show
    . stepToFinal
    . readBoard (0, 0) (Board Map.empty)

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs

readBoard _ board [] = board
readBoard (y, x) board ('\n':input) = readBoard (y+1, 0) board input
readBoard pos@(y,x) (Board board) (p:input) = board' `seq` readBoard (y,x+1) board' input
    where board' = Board $ Map.insert pos p board

boardMap (Board map) = map

getNeighbours (y, x) board =
   [ getSeat board (y-dy, x-dx)
   | dy <- [-1..1]
   , dx <- [-1..1]
   , dx /= 0 || dy /= 0
   ] 

getSeat (Board board) (y, x)
    | y <= -1 || x <= -1 || x >= nX || y >= nY = '.'
    | otherwise = board ! (y, x)

step board@(Board map) = Board $ Map.mapWithKey stepPlace map
    where stepPlace k v = let neighbours = getNeighbours k board in case v of
                            '.' -> '.'
                            'L' -> if '#' `notElem` neighbours
                                      then '#' else 'L'
                            '#' -> if (length. filter (=='#')) neighbours > 3
                                      then 'L' else '#'

stepToFinal board = if board == next then board else stepToFinal next
   where next = step board

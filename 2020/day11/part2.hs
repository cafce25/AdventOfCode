import Data.List hiding (insert)
import Data.List.Split
import Data.Map (Map, (!))
import System.Environment
import System.IO
import qualified Data.Map as Map

data Board = Board { rBoard :: Map (Int, Int) Char
                   , rHeight :: Int
                   , rWidth :: Int
                   } deriving (Eq)


instance Show Board where
    show Board{ rWidth = nX, rBoard = board} = intercalate "\n" . chunksOf nX . map snd . Map.toList $ board

main = getArgs
    >>= parse
    >>= print
    . length
    . filter (=='#')
    . show
    . stepToFinalFar
    . readBoard


parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs

readBoard input = readBoard' (0, 0) (Board Map.empty height width) input
    where width = length$ head l
          height = length l
          l = lines input
          readBoard' _ board [] = board
          readBoard' (y, x) board ('\n':input) = readBoard' (y+1, 0) board input
          readBoard' pos@(y,x) board@Board{rBoard=map} (p:input) = board' `seq` readBoard' (y,x+1) board' input
              where board' = board{rBoard = Map.insert pos p map}

boardMap Board{rBoard = map} = map

getSeat board@Board{rBoard = map} (y, x)
    | notInBounds board y x = '.'
    | otherwise = map ! (y, x)

getNeighbours (y, x) board = [
    getSeat board (y-dy, x-dx)
    | dy <- [-1..1]
    , dx <- [-1..1]
    , dx /= 0 || dy /= 0] 


notInBounds :: Board -> Int -> Int -> Bool
notInBounds b y = not . inBounds b y

inBounds :: Board -> Int -> Int -> Bool
inBounds Board{rWidth = w, rHeight = h} y x = y > -1 && x > -1 && x < w && y < h

getFarSeat board (dy, dx) (y, x)
    | inBounds board ny nx && seatHere == '.' = getFarSeat board (dy, dx) (ny, nx)
    | otherwise = seatHere
        where nx = x + dx
              ny = y + dy
              seatHere = getSeat board (ny, nx)
getFarNeighbours (y, x) board = [
    getFarSeat board (dy, dx) (y, x)
    | dy <- [-1..1]
    , dx <- [-1..1]
    , dx /= 0 || dy /= 0] 


step = step' getNeighbours
stepFar = step' getFarNeighbours
stepNFar 0 = id
stepNFar n = stepFar . stepNFar (n-1)
step' getN board@Board{rBoard = map} = board{rBoard = Map.mapWithKey stepPlace map}
    where stepPlace k v = let neighbours = getN k board in case v of
                            '.' -> '.'
                            'L' -> if '#' `notElem` neighbours
                                      then '#' else 'L'
                            '#' -> if (length. filter (=='#')) neighbours > 4
                                      then 'L' else '#'

stepToFinal = stepToFinal' step
stepToFinalFar = stepToFinal' stepFar
stepToFinal' s board = if board == next then board else stepToFinal' s next
   where next =  s board

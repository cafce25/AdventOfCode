{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = [[Move]]
data Move = E | SE | SW | W | NW | NE deriving Show
type Parser = Parsec Void String
type Coord = (Int, Int)
type Board = Set Coord


part1 :: Input -> Int
part1 = length . blackTiles

blackTiles :: [[Move]] -> [Coord]
blackTiles = map snd . filter ((==1) . (`mod` 2) . fst) . map (length &&& head) . group . sort . map coordAfterMoves

moveP :: Parser Move
moveP = choice
    [ E <$ char 'e'
    , SE <$ string "se"
    , SW <$ string "sw"
    , W <$ char 'w'
    , NW <$ string "nw"
    , NE <$ string "ne"
    ]

movesP :: Parser [Move]
movesP = some moveP

movesListP :: Parser [[Move]]
movesListP = movesP `endBy` eol

coordAfterMoves :: [Move] -> Coord
coordAfterMoves = foldr (flip move) (0, 0)

move :: (Num a) => (a, a) -> Move -> (a, a)
move (q, r) m = case m of
                  E ->  (q+1, r)
                  W ->  (q-1, r)
                  NE -> (q+1, r-1)
                  SW -> (q-1, r+1)
                  NW -> (q, r-1)
                  SE -> (q, r+1)

part2 :: Input -> Int
part2 = S.size . (!!100) . iterate step . S.fromList . blackTiles

neighbours :: Board -> Coord -> Int
neighbours b c = length $ filter ((`S.member` b) . move c) [E, NE, NW, W, SW, SE]

bounds :: Board -> (Coord, Coord)
bounds b = ((minimum qs, minimum rs), (maximum qs, maximum rs))
    where coords = S.toList b
          qs = map fst coords
          rs = map snd coords

step :: Board -> Board
step b = S.fromList $ [(q, r) | let ((minQ, minR), (maxQ, maxR)) = bounds b
                      , q <- [pred minQ..succ maxQ]
                      , r <- [pred minR..succ maxR]
                      , let n = neighbours b (q, r)
                      , if (q, r) `S.member` b
                           then n `elem` [1, 2]
                           else n == 2]
{-
 -rules:
 -     Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white
 -     Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black
 -}

prepare :: String -> Input
prepare = fromJust . parseMaybe movesListP

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

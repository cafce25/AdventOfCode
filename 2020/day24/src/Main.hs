{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Data.Maybe (fromJust)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char

type Input = [[Move]]
data Move = E | SE | SW | W | NW | NE deriving Show
type Parser = Parsec Void String
type Coord = (Int, Int)

part1 :: Input -> Int
part1 = length . filter ((==1) . (`mod` 2) . fst) . map (length &&& head) . group . sort . map coordAfterMoves

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
coordAfterMoves = coordAfterMoves' 0 0
    where coordAfterMoves' q r [] = (q, r)
          coordAfterMoves' q r (m:ms) = case m of
                                          E -> coordAfterMoves' (q+1) r ms
                                          W -> coordAfterMoves' (q-1) r ms
                                          NE -> coordAfterMoves' (q+1) (r-1) ms
                                          SW -> coordAfterMoves' (q-1) (r+1) ms
                                          NW -> coordAfterMoves' q (r-1) ms
                                          SE -> coordAfterMoves' q (r+1) ms

part2 :: Input -> ()
part2 = const ()

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

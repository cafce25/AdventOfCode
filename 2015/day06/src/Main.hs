{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Text.Read ( lift
                 , readListPrec
                 , readListPrecDefault
                 , readPrec
                 , readPrec_to_P)
import Text.ParserCombinators.ReadP
import System.Environment

type Input = [Instruction]
data Instruction = Turn{ s::LightStatus, fromR, toR :: Coordinate }
                 | Toggle{ fromR, toR :: Coordinate} deriving Show
type LightStatus = Bool
on :: Bool
on = True
off :: Bool
off = False

type Coordinate = (Int, Int)
type Grid = Map Coordinate LightStatus

instance Read Instruction where
    readPrec = lift readInstruction
    readListPrec = readListPrecDefault

readInstruction :: ReadP Instruction
readInstruction = readTurn +++ readToggle

readTurn :: ReadP Instruction
readTurn = do
    string "turn"
    skipSpaces
    onoff <- (   (string "on"  >> skipSpaces >> return True)
             +++ (string "off" >> skipSpaces >> return False))
    (from, to) <- readFromTo
    return $ Turn onoff from to


readToggle :: ReadP Instruction
readToggle = do
    string "toggle"
    skipSpaces
    (from, to) <- readFromTo
    skipSpaces
    return $ Toggle from to

readFromTo :: ReadP (Coordinate, Coordinate)
readFromTo = do
    x1 <- readPrec_to_P readPrec 0
    char ','
    y1 <- readPrec_to_P readPrec 0
    skipSpaces
    string "through"
    skipSpaces
    x2 <- readPrec_to_P readPrec 0
    char ','
    y2 <- readPrec_to_P readPrec 0
    return ((x1,y1), (x2,y2))

turn :: LightStatus -> [Coordinate] -> Grid -> Grid
turn onoff = flip $ foldl' (\g c -> g `seq` M.insert c onoff g)

toggle :: [Coordinate] -> Grid -> Grid
toggle = flip $ foldl' (flip $ M.adjust not)

through :: Coordinate -> Coordinate -> [Coordinate]
through (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

allLit :: Grid
allLit = turn on ((0,0) `through` (999,999)) M.empty
noneLit :: Grid
noneLit = turn off ((0,0) `through` (999,999)) M.empty

run :: Input -> Grid
run = foldl go noneLit
    where go grid (Toggle f t) = toggle (f `through` t) grid
          go grid (Turn onoff f t) = turn onoff (f `through` t) grid

part1 :: Input -> Int
part1 = const length. filter id. M.elems. run

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map read. lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs


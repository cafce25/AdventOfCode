module Common where

import Text.Read hiding ((+++))
import Text.ParserCombinators.ReadP

type Input = [Instruction]
data Instruction = Turn{ s::LightStatus, fromR, toR :: Coordinate }
                 | Toggle{ fromR, toR :: Coordinate} deriving Show

type LightStatus = Bool
on :: Bool
on = True
off :: Bool
off = False

type Coordinate = (Int, Int)

instance Read Instruction where
    readPrec = lift readInstruction
    readListPrec = readListPrecDefault

readInstruction :: ReadP Instruction
readInstruction = readTurn +++ readToggle

readTurn :: ReadP Instruction
readTurn = do
    _ <- string "turn"
    skipSpaces
    onoff <-     (string "on"  >> skipSpaces >> return True )
             +++ (string "off" >> skipSpaces >> return False)
    (from, to) <- readFromTo
    return $ Turn onoff from to


readToggle :: ReadP Instruction
readToggle = do
    _ <- string "toggle"
    skipSpaces
    (from, to) <- readFromTo
    skipSpaces
    return $ Toggle from to

readFromTo :: ReadP (Coordinate, Coordinate)
readFromTo = do
    x1 <- readPrec_to_P readPrec 0
    _ <- char ','
    y1 <- readPrec_to_P readPrec 0
    skipSpaces
    _ <- string "through"
    skipSpaces
    x2 <- readPrec_to_P readPrec 0
    _ <- char ','
    y2 <- readPrec_to_P readPrec 0
    return ((x1,y1), (x2,y2))

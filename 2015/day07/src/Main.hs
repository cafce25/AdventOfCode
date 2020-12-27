{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import Data.Bits
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as M
import System.Environment
import Text.ParserCombinators.ReadP
import Text.Read ( lift
                 , readListPrec
                 , readListPrecDefault
                 , readPrec
                 , readPrec_to_P)


type Input = [Instruction]
type Diagram = Map String Int
type Port = String
data Instruction = And{ fstP:: Either Port Int, sndP, dest ::Port}
                 | Or{ fstP:: Either Port Int, sndP, dest ::Port}
                 | Not{ fstP:: Either Port Int, dest ::Port}
                 | Shift{ fstP:: Either Port Int, src, dest::Port}
                 | Assign{ fstP:: Either Port Int, dest::Port }
                 deriving Show
type BinaryInstructionCons = Either Port Int -> Port -> Port -> Instruction

rShift :: BinaryInstructionCons
rShift (Right a) = Shift (Right (-a))
rShift _ = undefined

instance Read Instruction where 
    readPrec = lift readInstruction
    readListPrec = readListPrecDefault

readInstruction :: ReadP Instruction
readInstruction =   readBinary "AND"   And
                +++ readBinary "OR"    Or
                +++ readBinary "LSHIFT" Shift
                +++ readBinary "RSHIFT" rShift
                +++ readNot
                +++ readAssign


readNot :: ReadP Instruction
readNot = do
    _ <- string "NOT"
    skipSpaces
    Assign fstPort destPort <- readAssign
    return $ Not fstPort destPort

readAssign :: ReadP Instruction
readAssign = do
    fstPort <- readEitherPortOrImm
    skipAssignOp
    Assign fstPort <$> readPort

readBinary :: String -> BinaryInstructionCons -> ReadP Instruction
readBinary op gen = do
    (fstPort, sndPort) <- readOp op
    skipAssignOp
    gen fstPort sndPort <$> readPort

readOp :: String -> ReadP (Either Port Int, Port)
readOp op = (do
        fstPort <- readEitherPortOrImm
        skipOp op
        sndPort <- readPort
        return (fstPort, sndPort))
    <++ (do
        sndPort <- readPort
        skipOp op
        fstPort <- readEitherPortOrImm
        return (fstPort, sndPort))
    

skipOp :: String -> ReadP ()
skipOp op = do
    skipSpaces
    _ <- string op
    skipSpaces

skipAssignOp :: ReadP ()
skipAssignOp = do
    skipSpaces
    _ <- string "->"
    skipSpaces

readEitherPortOrImm :: ReadP (Either Port Int)
readEitherPortOrImm = do Left <$> readPort
                  +++ do Right <$> readImm

readPort :: ReadP Port
readPort = many1 $ satisfy (`elem` ['a'..'z'])

readImm :: ReadP Int
readImm = readPrec_to_P readPrec 0


getPortsTable :: Input -> Diagram
getPortsTable instructions = table
    where table = M.fromList [case ins of 
            (Assign (Right i)   d) -> (d, i)
            (Assign (Left  p)   d) -> (d, table!p)
            (And    (Left fp) p d) -> (d, table!fp .&. table!p)
            (And    (Right i) p d) -> (d, i .&. table!p)
            (Or     (Left fp) p d) -> (d, table!fp .|. table!p)
            (Or     (Right i) p d) -> (d, i .|. table!p)
            (Not    (Left  p)   d) -> (d, complement $ table!p)
            (Not    (Right i)   d) -> (d, complement i)
            (Shift  (Right i) p d) -> (d, shift (table!p) i)
            (Shift  (Left sp) p d) -> (d, shift (table!p) (table!sp))
                | ins <- instructions]

part1 :: Input -> Int
part1 ins = getPortsTable ins ! "a"

part2 :: Input -> Int
part2 ins = getPortsTable (ins ++ [Assign (Right 3176) "b"]) ! "a"

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

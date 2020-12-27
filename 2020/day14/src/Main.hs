{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow                ((&&&))
import Data.Bits                    (complement, shift, (.&.), (.|.))
import Data.IntMap                  (IntMap, elems, empty, insert)
import System.Environment           (getArgs)
import Text.ParserCombinators.ReadP (ReadP, munch, string, (<++))
import Text.Read
    (lift, readListPrec, readListPrecDefault, readPrec, readPrec_to_P)

data Instruction = Mask [Bit]
                 | Assign { address, value :: Int
                          }
  deriving Show
data Bit = One | Zero | Undefined deriving (Eq, Show)

type Input = [Instruction]

instance Read Instruction where
    readPrec = lift (readMask <++ readAssign)
    readListPrec = readListPrecDefault

readMask :: ReadP Instruction
readMask = do
    _ <- string "mask = "
    maskStr <- munch (const True)
    pure $ Mask (map charToBit maskStr)
        where charToBit '1' = One
              charToBit '0' = Zero
              charToBit _   = Undefined

readAssign :: ReadP Instruction
readAssign = do
    _ <- string "mem["
    adr <- readPrec_to_P readPrec 0
    _ <- string "] = "
    val <- readPrec_to_P readPrec 0
    pure $ Assign adr val


set :: Instruction -> Int
set (Mask mask) = maskFromBits 0 mask
    where maskFromBits :: Int -> [Bit] -> Int
          maskFromBits s []              = s
          maskFromBits s (Undefined:str) = maskFromBits (shift s 1) str
          maskFromBits s (One:str)       = maskFromBits (succ (shift s 1)) str
          maskFromBits s (Zero:str)      = maskFromBits (shift s 1) str
set _ = undefined

reset :: Instruction -> Int
reset (Mask mask) = maskFromBits 0 mask
    where maskFromBits :: Int -> [Bit] -> Int
          maskFromBits r []              = r
          maskFromBits r (Undefined:str) = maskFromBits (succ (shift r 1)) str
          maskFromBits r (One:str)       = maskFromBits (succ (shift r 1)) str
          maskFromBits r (Zero:str)      = maskFromBits (shift r 1) str
reset _ = undefined

part1 :: Input -> Int
part1 = sum . elems . run
    where run = go (Mask []) empty
          go :: Instruction -> IntMap Int -> [Instruction] -> IntMap Int
          go _ table [] = table
          go _ table (mask@Mask{}:ins) = go mask table ins
          go mask table ((Assign addr val):ins) =
                  go mask (insert addr (set mask .|. val .&. reset mask) table) ins


fluctuateAdress :: [Bit] -> Int -> [Int]
fluctuateAdress [] addr = [addr]
fluctuateAdress (b:mask) addr
  | b == One  =  fluctuateAdress mask (addr .|. shift 1 sh)
  | b == Zero =  fluctuateAdress mask addr
  | otherwise =  fluctuateAdress mask (addr .|. shift 1 sh)
              ++ fluctuateAdress mask (addr .&. complement (shift 1 sh))
    where sh = length mask

part2 :: Input -> Int
part2 = sum . elems . run
    where run = go [] empty
          go :: [Bit] -> IntMap Int -> [Instruction] -> IntMap Int
          go _ table [] = table
          go _ table ((Mask mask):ins) = go mask table ins
          go mask table ((Assign addr val):ins) = go mask table' ins
              where table' = foldl (\m a -> insert a val m) table (fluctuateAdress mask addr)

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

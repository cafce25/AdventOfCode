module Main where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Scientific (toBoundedInteger)
import Data.Foldable (toList)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust)
import System.Environment (getArgs)

type Input = Value

numbers :: Value -> [Int]
numbers (Array a) = concatMap numbers a
numbers (Object o) = concatMap numbers $ toList o
numbers (Number i) = [fromJust $ toBoundedInteger i]
numbers (String _) = []
numbers (Bool _) = []
numbers Null = []

part1 :: Input -> Int
part1 = sum . numbers

part2 :: Input -> ()
part2 = const ()

prepare :: B.ByteString -> Input
prepare = fromJust . decode

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO B.ByteString
getInputContents = do
    args <- getArgs
    case args of
        []    -> B.readFile "input"
        ["-"] -> B.getContents
        fs    -> B.concat `fmap` mapM B.readFile fs

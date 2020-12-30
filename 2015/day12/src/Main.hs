{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ((&&&))
import Data.Aeson
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

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

numbers' :: Value -> [Int]
numbers' (Object o) = if "red" `elem` l then [] else concatMap numbers' l
    where l = toList o
numbers' (Array a) = concatMap numbers' a
numbers' (Number i) = [fromJust $ toBoundedInteger i]
numbers' (String _) = []
numbers' (Bool _) = []
numbers' Null = []

part2 :: Input -> Int
part2 = sum . numbers'

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

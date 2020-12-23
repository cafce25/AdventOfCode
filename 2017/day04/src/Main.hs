{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (sort)
import System.Environment (getArgs)

type Input = [[String]]

checkPP :: [String] -> Bool
checkPP = checkPP' . sort 

checkPP' :: [String] -> Bool
checkPP' (x:y:xs) = if x==y then False else checkPP' (y:xs)
checkPP' _ = True 
          
    
part1 :: Input -> Int
part1 = length . filter checkPP

checkPP2 :: [String] -> Bool
checkPP2 = checkPP' . sort . map sort

part2 :: Input -> Int
part2 = length . filter checkPP2

prepare :: String -> Input
prepare = map words . lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare


getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (sort)
import System.Environment (getArgs)

type Input = [[String]]

checkPP :: [String] -> Bool
checkPP = checkPP' . sort 
    where checkPP' (x:y:xs) = if x==y then False else checkPP' (y:xs)
          checkPP' _ = True 
          
    
part1 :: Input -> Int
part1 = length . filter checkPP

part2 :: Input -> ()
part2 = const ()

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

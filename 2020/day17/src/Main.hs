module Main where

import Common
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M
import Part1 (part1)
import Part2 (part2)
import System.Environment (getArgs)

prepare :: String -> Input
prepare input = M.fromList [((x, y), v == '#')| (x, l) <- withIndices, (y, v) <- l]
    where withIndices = zip [0..] . map (zip [0..]) . lines $ input

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

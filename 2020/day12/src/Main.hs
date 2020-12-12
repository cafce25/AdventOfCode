{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import System.Environment


data Action = Action Dir Int deriving Show
type Dir = Char
type Position = (Int, Int)
type Input = [Action]


followDirections :: Position -> Dir -> [Action] -> [Int]
followDirections (y, x) _ [] = [y, x]
followDirections pos@(y, x) dir (i@(Action a d):instructions)
    | a == 'F' = followDirections pos dir (Action dir d:instructions)
    | a == 'N' = followDirections (y+d, x) dir instructions
    | a == 'S' = followDirections (y-d, x) dir instructions
    | a == 'E' = followDirections (y, x+d) dir instructions
    | a == 'W' = followDirections (y, x-d) dir instructions
    | otherwise = followDirections pos (rotate dir i) instructions

rotate :: Dir -> Action -> Dir
rotate d (Action 'L' angle) = rotate d (Action 'R' (angle * 3))
rotate d (Action _ angle) = directions d !! quarterTurns   
    where quarterTurns = (angle `div` 90) `mod` 4
          directions c = dropWhile (/= c) $ cycle "NESW"

part1 :: Input -> Int
part1 = sum. map abs. followDirections (0, 0) 'E'

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (\x -> Action (head x) (read $ tail x)) . lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

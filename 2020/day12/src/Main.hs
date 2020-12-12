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

followWaypoint :: Position -> Position -> [Action] -> [Int]
followWaypoint (y, x) _ [] = [y, x]
followWaypoint pos@(y, x) wp@(wy, wx) (i@(Action a d):instructions)
    | a == 'F' = followWaypoint (y+d*wy, x+d*wx) wp instructions
    | a == 'N' = followWaypoint pos (wy+d, wx) instructions
    | a == 'S' = followWaypoint pos (wy-d, wx) instructions
    | a == 'E' = followWaypoint pos (wy, wx+d) instructions
    | a == 'W' = followWaypoint pos (wy, wx-d) instructions
    | otherwise = followWaypoint pos (rotateWaypoint wp i) instructions

rotateWaypoint :: Position -> Action -> Position
rotateWaypoint p (Action 'L' angle) = rotateWaypoint p (Action 'R' (angle * 3))
rotateWaypoint (y, x) (Action _ angle) = (x * sinA + y * cosA, x * cosA - y * sinA)
    where angleRad :: Double
          angleRad = (fromIntegral angle * pi) / 180
          sinA :: Int
          sinA = round $ sin (-angleRad)
          cosA :: Int
          cosA = round $ cos angleRad

part1 :: Input -> Int
part1 = sum. map abs. followDirections (0, 0) 'E'

part2 :: Input -> Int
part2 = sum. map abs. followWaypoint (0, 0) (1, 10)

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

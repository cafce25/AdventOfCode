{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Part2 where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Common

type Grid = Map Coordinate Int

turn :: LightStatus -> [Coordinate] -> Grid -> Grid
turn onoff = flip $ foldl' (\g c -> M.adjust (\x -> if onoff then x+1 else max (x-1) 0) c g)

toggle :: [Coordinate] -> Grid -> Grid
toggle = flip $ foldl' (flip $ M.adjust (+2))

through :: Coordinate -> Coordinate -> [Coordinate]
through (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

allOff :: Grid
allOff = foldl' (\g k -> M.insert k 0 g) M.empty ((0,0) `through` (999,999))

run :: Input -> Grid
run = foldl go allOff
    where go grid (Toggle f t) = toggle (f `through` t) grid
          go grid (Turn onoff f t) = turn onoff (f `through` t) grid

part2 :: Input -> Int
part2 = sum. M.elems. run

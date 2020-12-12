{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Part1 where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Common

type Grid = Map Coordinate LightStatus

turn :: LightStatus -> [Coordinate] -> Grid -> Grid
turn onoff = flip $ foldl' (\g c -> g `seq` M.insert c onoff g)

toggle :: [Coordinate] -> Grid -> Grid
toggle = flip $ foldl' (flip $ M.adjust not)

through :: Coordinate -> Coordinate -> [Coordinate]
through (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

allLit :: Grid
allLit = turn on ((0,0) `through` (999,999)) M.empty
noneLit :: Grid
noneLit = turn off ((0,0) `through` (999,999)) M.empty

run :: Input -> Grid
run = foldl go noneLit
    where go grid (Toggle f t) = toggle (f `through` t) grid
          go grid (Turn onoff f t) = turn onoff (f `through` t) grid

part1 :: Input -> Int
part1 = length. filter id. M.elems. run

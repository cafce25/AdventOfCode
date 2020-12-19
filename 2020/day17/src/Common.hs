{-# LANGUAGE RankNTypes #-}
module Common where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Input = Map (Int, Int) Bool
type Cell = Bool

type Grid c = Map c Cell
type GetCellNext a = Coord a => Grid a-> a -> Cell -> Cell

class Coord c where
    add :: c -> c -> c
    getArea :: c -> c -> [c]
    extrema :: [c] -> (c, c)
    neighbourCoords :: c -> [c]

stepGrid :: (Ord a, Coord a) => Grid a -> Grid a
stepGrid g = foldr s M.empty (uncurry getArea. getBounds $ g)
    where s c g'= M.insert c (stepCell g c v) g'
              where v = M.findWithDefault False c g

stepCell :: (Ord a, Coord a) => Grid a -> a -> Cell -> Cell
stepCell g c v = nNeighbours == 3 || v && nNeighbours == 2
    where neighbours = map (\k -> M.findWithDefault False k g) (neighbourCoords c)
          nNeighbours = length (filter id neighbours)

getBounds :: Coord a => Grid a -> (a, a)
getBounds = extrema. M.keys

aliveN :: Grid a -> Int
aliveN = length . filter snd . M.toList

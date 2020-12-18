module Part2 (part2) where

import           Common
import           Data.List (unzip4)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Coord = (Int, Int, Int, Int)
type Grid = Map Coord Cell
type GetNeighbours = Coord -> [Coord]
type GetCellNext = GetNeighbours -> Grid -> Coord -> Cell -> Cell

stepGrid :: GetNeighbours -> GetCellNext -> Grid -> Grid
stepGrid n sc g = foldr s M.empty (uncurry getArea. getBounds $ g)
    where s :: Coord -> Grid -> Grid
          s c g'= M.insert c (sc n g c v) g'
              where v = M.findWithDefault False c g

stepCell :: GetCellNext
stepCell n g c v = if v
                    then length (filter id neighbours) `elem` [2..3]
                    else length (filter id neighbours) == 3
    where neighbours = map (\k -> M.findWithDefault False k g) (n c)

neighbourCoords :: GetNeighbours
neighbourCoords p = tail [add p (x, y, z, w)| x <- r , y <- r , z <- r, w <- r]
    where add (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1+x2 , y1+y2 , z1+z2, w1+w2)
          r = [0, 1, -1] -- ordering is important to filter out (0, 0, 0)

getArea :: Coord -> Coord -> [Coord]
getArea (x1, y1, z1, w1) (x2, y2, z2, w2) = [(x, y, z, w)| x <- [pred x1..succ x2]
                                                 , y <- [pred y1..succ y2]
                                                 , z <- [pred z1..succ z2]
                                                 , w <- [pred w1..succ w2]]

getBounds :: Grid -> (Coord, Coord)
getBounds = extrema. unzip4. M.keys
    where extrema (x, y, z, w) = ((minimum x, minimum y, minimum z, minimum w)
                              ,(maximum x, maximum y, maximum z, maximum w)
                              ) 

aliveN :: Grid -> Int
aliveN = length . filter snd . M.toList

inputToGrid :: Input -> Grid
inputToGrid input = M.fromList [((x, y, 0, 0), v) | ((x, y), v) <- input]

part2 :: Input -> Int
part2 = aliveN . (!!6) . iterate step . inputToGrid
    where step = stepGrid neighbourCoords stepCell

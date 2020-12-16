module Main where

import Control.Arrow ((&&&))
import System.Environment (getArgs)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Coord = (Int, Int, Int)
type Cell = Bool
type Input = Grid
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
neighbourCoords p = tail [add p (x, y, z)| x <- r , y <- r , z <- r]
    where add (x1, y1, z1) (x2, y2, z2) = (x1+x2 , y1+y2 , z1+z2)
          r = [0, 1, -1] -- ordering is important to filter out (0, 0, 0)

getArea :: Coord -> Coord -> [Coord]
getArea (x1, y1, z1) (x2, y2, z2) = [(x, y, z)| x <- [pred x1..succ x2]
                                                 , y <- [pred y1..succ y2]
                                                 , z <- [pred z1..succ z2]]

getBounds :: Grid -> (Coord, Coord)
getBounds = extrema. unzip3. M.keys
    where extrema (a, b, c) = ((minimum a, minimum b, minimum c)
                              ,(maximum a, maximum b, maximum c)
                              ) 

aliveN :: Grid -> Int
aliveN = length . filter snd . M.toList

part1 :: Input -> Int
part1 = aliveN . (!!6) . iterate step
    where step = stepGrid neighbourCoords stepCell

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare input = M.fromList [((x, y, 0), v == '#')| (x, l) <- withIndices, (y, v) <- l]
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

module Part1 (part1) where

import           Common
import qualified Data.Map.Strict as M

newtype Coord3 = Coord3 (Int, Int, Int) deriving (Ord, Eq, Show)

instance Coord Coord3 where
    add (Coord3 (x1, y1, z1)) (Coord3 (x2, y2, z2)) = Coord3 (x1+x2 , y1+y2 , z1+z2)
    getArea (Coord3 (x1, y1, z1)) (Coord3 (x2, y2, z2)) =
        [ Coord3 (x, y, z)
        | x <- [pred x1..succ x2]
        , y <- [pred y1..succ y2]
        , z <- [pred z1..succ z2]
        ]

    neighbourCoords p = tail [add p (Coord3 (x, y, z))| x <- r , y <- r , z <- r]
        where r = [0, 1, -1] -- ordering is important to filter out (0, 0, 0)

    extrema coords = extrema' $ unzip3 coords'
        where coords' = map (\(Coord3 c) -> c) coords
              extrema' (a, b, c) = ( Coord3 ( minimum a, minimum b, minimum c)
                                   , Coord3 ( maximum a, maximum b, maximum c)
                                   )


inputToGrid :: Input -> Grid Coord3
inputToGrid = M.mapKeys (\(x, y) -> Coord3 (x, y, 0))

part1 :: Input -> Int
part1 = aliveN . (!!6) . iterate stepGrid . inputToGrid

module Part2 (part2) where

import           Common
import           Data.List (unzip4)
import qualified Data.Map.Strict as M

newtype Coord4 = Coord4 (Int, Int, Int, Int) deriving (Ord, Eq, Show)

instance Coord Coord4 where
    add (Coord4 (x1, y1, z1, w1)) (Coord4 (x2, y2, z2, w2)) = Coord4 (x1+x2 , y1+y2 , z1+z2, w1+w2)
    getArea (Coord4 (x1, y1, z1, w1)) (Coord4 (x2, y2, z2, w2)) =
        [ Coord4 (x, y, z, w)
        | x <- [pred x1..succ x2]
        , y <- [pred y1..succ y2]
        , z <- [pred z1..succ z2]
        , w <- [pred w1..succ w2]
        ]

    neighbourCoords p = tail [add p (Coord4 (x, y, z, w))| x <- r , y <- r , z <- r, w <- r]
        where r = [0, 1, -1] -- ordering is important to filter out (0, 0, 0)

    extrema coords = extrema' $ unzip4 coords'
        where coords' = map (\(Coord4 c) -> c) coords
              extrema' (a, b, c, d) =
                  ( Coord4 ( minimum a, minimum b, minimum c, minimum d)
                  , Coord4 ( maximum a, maximum b, maximum c, maximum d)
                  )

inputToGrid :: Input -> Grid Coord4
inputToGrid = M.mapKeys (\(x, y) -> Coord4 (x, y, 0, 0))

part2 :: Input -> Int
part2 = aliveN . (!!6) . iterate stepGrid . inputToGrid

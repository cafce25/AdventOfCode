import Data.List.Unique

main = print . length . sortUniq . concat . map (deliver (0, 0) []) . distribute =<< readFile "input"

distribute xs = distribute' ("", "") xs
    where distribute' (xs, ys) [] = [reverse xs, reverse ys]
          distribute' (xs, ys) [x] = distribute' (x:xs, ys) []
          distribute' (xs, ys) (x:y:zs) = distribute' (x:xs, y:ys) zs
deliver pos poss [] = pos:poss
deliver pos@(x, y) poss (dir:dirs) = case dir of
    '^' -> deliver (x, y-1) npos dirs
    'v' -> deliver (x, y+1) npos dirs
    '<' -> deliver (x-1, y) npos dirs
    '>' -> deliver (x+1, y) npos dirs
    _ -> deliver pos poss dirs
    where npos = pos:poss

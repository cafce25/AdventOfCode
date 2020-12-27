import Data.List.Unique

main = print . length . sortUniq . deliver (0, 0) [] =<< readFile "input"

deliver pos poss [] = pos:poss
deliver pos@(x, y) poss (dir:dirs) = case dir of
    '^' -> deliver (x, y-1) npos dirs
    'v' -> deliver (x, y+1) npos dirs
    '<' -> deliver (x-1, y) npos dirs
    '>' -> deliver (x+1, y) npos dirs
    _ -> deliver pos poss dirs
    where npos = pos:poss

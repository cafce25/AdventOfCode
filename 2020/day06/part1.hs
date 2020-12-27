import Data.List.Split
import Data.List.Unique

main = interact $ show . length . concatMap (sortUniq . concat) . splitOn [""] . lines

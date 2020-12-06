import Data.List.Split
import Data.List.Unique

main = interact $ show . length . concat . map sortUniq . map concat . splitOn [""] . lines



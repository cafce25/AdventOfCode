import Data.Sort
import Data.List.Split

main = print . sum . map (wrappingNeeded . readLine) . lines =<< readFile "input"

readLine :: String -> [Int]
readLine = map read . splitOn "x"
wrappingNeeded [x, y, z] = 3 * min * mid + 2 * min * max + 2 * mid * max
    where [min, mid, max] = sort [x, y, z]

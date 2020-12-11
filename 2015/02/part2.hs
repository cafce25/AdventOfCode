import Data.Sort
import Data.List.Split

main = print . sum . map (ribbonNeeded . readLine) . lines =<< readFile "input"

readLine :: String -> [Int]
readLine = map read . splitOn "x"
ribbonNeeded [x, y, z] = 2 * min + 2 * mid + min * mid * max
    where [min, mid, max] = sort [x, y, z]

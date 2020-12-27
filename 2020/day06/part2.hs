import Data.List.Split
import Data.List.Unique

main = interact $ show . length . concatMap allIn . splitOn [""] . lines

allIn :: [String] -> String
allIn xs = filter (\x -> all (elem x) xs) ['a'..'z']

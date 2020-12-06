import Data.List.Split
import Data.List.Unique

main = interact $ show . length . concat . map allIn . splitOn [""] . lines


allIn :: [String] -> String
allIn xs = filter (\x -> all (elem x) xs) ['a'..'z']



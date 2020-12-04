module Main where

import qualified Data.Map as Map
import Data.List.Split

main = interact $ show . length . filter validPassport . normalize . (map words) . lines

type Passport = Map.Map String String

requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
optionalKeys = ["cid"]

--normalize :: [[String]] -> [[String]]
normalize :: [[String]] -> [Passport]
normalize = (map ( parsePassport . concat)) . splitOn [[]]

parsePassport = Map.fromList . map ((\[k,v] -> (k,v)) . splitOn ":")

validPassport :: Passport -> Bool
validPassport p = length requiredKeys == length validPassportKeys
    where
        passportKeys = Map.keys p
        validPassportKeys = filter (`elem` requiredKeys) passportKeys


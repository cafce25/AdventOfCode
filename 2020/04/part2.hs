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

--creating a Map filters duplicate keys
parsePassport = Map.fromList . map ((\[k,v] -> (k,v)) . splitOn ":")

validPassportKey :: String -> String -> Bool
validPassportKey el = case el of
    "byr" -> validBirthYear
    "iyr" -> validIssueYear
    "eyr" -> validExpirationYear
    "hgt" -> validHeight
    "hcl" -> validHairColor
    "ecl" -> validEyeColor
    "pid" -> validPassportId
    _ -> const False

validYear start end year = length year == 4 && start <= read year && end >= read year
validBirthYear = validYear 1920 2002
validIssueYear = validYear 2010 2020
validExpirationYear = validYear 2020 2030

validHeight height = case u of
    "cm" -> h >= 150 && h <= 193
    "in" -> h >= 59 && h <= 76
    _ -> False
    where
        (hStr, u) = break (not . (`elem` ['0'..'9'])) height
        h = read hStr

validHairColor ('#':hcl) = length hcl == 6 && all (`elem` hexDigits) hcl
validHairColor _ = False

hexDigits = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

validEyeColor "amb" = True
validEyeColor "blu" = True
validEyeColor "brn" = True
validEyeColor "gry" = True
validEyeColor "grn" = True
validEyeColor "hzl" = True
validEyeColor "oth" = True
validEyeColor _ = False

validPassportId pid = length pid == 9 && all (`elem`['0'..'9']) pid

validPassport :: Passport -> Bool
validPassport p = length requiredKeys == length validPassportKeys
    where
        validPassportKeys = Map.filterWithKey (validPassportKey) p
        --passportKeys = Map.keys p

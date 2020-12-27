module Main where

import qualified Data.Map as Map
import Data.List.Split

main = interact
     $ show . length . filter validPassport . normalize . map words . lines

type Passport = Map.Map String String

requiredKeys, optionalKeys :: [String]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
optionalKeys = ["cid"]

normalize :: [[String]] -> [Passport]
normalize = map ( parsePassport . concat) . splitOn [[]]

--creating a Map filters duplicate keys
parsePassport :: [String] -> Map.Map String String
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


validYear :: Int -> Int -> String -> Bool
validYear start end y = start <= year && end >= year
    where year = read y

validBirthYear, validIssueYear, validExpirationYear :: String -> Bool
validBirthYear = validYear 1920 2002
validIssueYear = validYear 2010 2020
validExpirationYear = validYear 2020 2030

validHeight :: String -> Bool
validHeight height = case u of
    "cm" -> h >= 150 && h <= 193
    "in" -> h >= 59 && h <= 76
    _ -> False
    where
        (hStr, u) = span (`elem` ['0'..'9']) height
        h = read hStr

validHairColor :: String -> Bool
validHairColor ('#':hcl) = length hcl == 6 && all (`elem` hexDigits) hcl
validHairColor _ = False

hexDigits :: [Char]
hexDigits = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

validEyeColor :: String -> Bool
validEyeColor "amb" = True
validEyeColor "blu" = True
validEyeColor "brn" = True
validEyeColor "gry" = True
validEyeColor "grn" = True
validEyeColor "hzl" = True
validEyeColor "oth" = True
validEyeColor _ = False

validPassportId :: String -> Bool
validPassportId pid = length pid == 9 && all (`elem`['0'..'9']) pid

validPassport :: Passport -> Bool
validPassport p = length requiredKeys == length validPassportKeys
    where
        validPassportKeys = Map.filterWithKey validPassportKey p

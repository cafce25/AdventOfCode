module Main where

import Text.Read.Lex
import Text.Read

main = interact $ show . countValidPws . readPws

newtype PasswordLine = PasswordLine (Integer, Integer, Char, String) deriving (Show)

countValidPws = foldr (\x y -> if validPw x then y + 1 else y) 0

instance Read PasswordLine where
    readPrec = do
        min <- step readPrec
        Symbol "-" <- lexP
        max <- step readPrec
        Ident cs <- lexP
        let [c] = cs
        Symbol ":" <- lexP
        Ident pw <- lexP
        return $ PasswordLine (min, max, c, pw)

readPws :: String -> [PasswordLine]
readPws = map read . lines

validPw :: PasswordLine -> Bool
validPw (PasswordLine (low, high, char, pw)) = nChars >= low && nChars <= high
    where nChars = countChar char pw

countChar :: Char -> String -> Integer
countChar char = foldr (\x y -> if x == char then 1 + y else y) 0

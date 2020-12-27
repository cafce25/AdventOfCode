module Main where

import Text.Read.Lex
import Text.Read

main = interact $ show . countValidPws . readPws

newtype PasswordLine = PasswordLine (Int, Int, Char, String) deriving (Show)

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
validPw (PasswordLine (pos1, pos2, char, pw)) = (char == pw !! (pos1-1)) /= (char == pw !! (pos2-1))

countChar :: Char -> String -> Int
countChar char = foldr (\x y -> if x == char then 1 + y else y) 0

{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Char (chr, isHexDigit)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Numeric (readHex)
import System.Environment (getArgs)
import Text.Megaparsec ( Parsec
                       , choice
                       , between
                       , many
                       , parseMaybe
                       , satisfy
                       , try
                       )
import Text.Megaparsec.Char (char, string)

type Input = [String]
type Parser = Parsec Void String

stringP :: Parser String
stringP = between (char '"') (char '"') escapedStringContents

escapedStringContents :: Parser String
escapedStringContents = many $ choice [ escape, nonEscape ]

escape :: Parser Char
escape = try $ char '\\' >> choice [ hexEscaped
                             , dquoteEscaped
                             , bsEscaped
                             ]

hexEscaped :: Parser Char
hexEscaped
  = chr . fst . head . readHex
  <$> (char 'x' *> replicateM 2 (satisfy isHexDigit))

dquoteEscaped :: Parser Char
dquoteEscaped = '"' <$ string "\""

bsEscaped :: Parser Char
bsEscaped = '\\' <$ string "\\"

nonEscape :: Parser Char
nonEscape = satisfy (/= '"')

part1 :: Input -> Int
part1 input = codeLength - memoryUsed
    where codeLength = sum . map length $ input
          memoryUsed
            = sum . map ( length
                        . fromMaybe (error "did not parse")
                        . parseMaybe stringP
                        )
            $ input

escapeS :: String -> String
escapeS u = '"' : go u ++ "\""
    where go [] = []
          go ('\\':xs) = "\\\\" ++ go xs
          go ('"' :xs) = "\\\"" ++ go xs
          go (x   :xs) = x:go xs

part2 :: Input -> Int
part2 input = escapedLength - codeLength
    where codeLength = sum . map length $ input
          escapedLength = sum . map (length . escapeS) $ input

prepare :: String -> Input
prepare = lines

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

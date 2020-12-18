{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Arrow ((&&&))
import           Control.Monad.Combinators.Expr
import           Data.Either (rights)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           System.Environment (getArgs)
import           Text.Megaparsec hiding (getInput)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Input = [Text]
type Parser = Parsec Void Text
data Expr = Int Int
          | Sum     Expr Expr
          | Product Expr Expr
          deriving (Eq, Ord, Show)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

value :: Expr -> Int
value (Int i) = i
value (Sum     e1 e2) = value e1 + value e2
value (Product e1 e2) = value e1 * value e2

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

pExpr :: [[Operator Parser Expr]] -> Parser Expr
pExpr opTable = pExpr'
    where pExpr' = makeExprParser pTerm opTable
          pTerm = choice
              [ parens pExpr'
              , pInteger
              ]

pExprSame, pExprPlusMul :: Parser Expr
pExprSame = pExpr operatorTableSame
pExprPlusMul = pExpr operatorTablePlusMul

operatorTableSame :: [[Operator Parser Expr]]
operatorTableSame =
    [ [ binary "*" Product
      , binary "+" Sum
      ]
    ]

operatorTablePlusMul :: [[Operator Parser Expr]]
operatorTablePlusMul =
    [ [ binary "+" Sum ]
    , [ binary "*" Product ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

parseSum :: Parser Expr -> Input -> Int
parseSum p = sum. map value. rights. map (parse p "")

part1 :: Input -> Int
part1 = parseSum pExprSame 

part2 :: Input -> Int
part2 = parseSum pExprPlusMul

prepare :: String -> Input
prepare = map (T.pack) . lines 

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

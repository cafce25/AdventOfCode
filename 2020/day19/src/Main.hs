{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Either
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           System.Environment (getArgs)
import           Text.Megaparsec hiding (getInput, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Input = ([Rule], [Text])
type Parser = Parsec Void Text
data Rule = Rule Varname Expr deriving (Ord, Eq, Show)
type Varname = Int
data Expr
    = Sum Expr Expr
    | Prod Expr Expr
    | Char Text
    | Variable Varname 
    deriving (Ord, Eq, Show)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

getParser :: IntMap Expr -> Expr -> Parser Text
getParser _ (Char s) = try (symbol s)
getParser varTable (Variable i) = try (getParser varTable (varTable M.! i))
getParser varTable (Sum a b)  = try (getParser varTable b) <|> try (getParser varTable a)
getParser varTable (Prod a b) = T.append <$> try (getParser varTable a) <*> try (getParser varTable b)

pVarname :: Parser Varname
pVarname = lexeme L.decimal

pVar :: Parser Expr
pVar = Variable <$> pVarname

pChar :: Parser Expr
pChar = Char . T.singleton <$> between (symbol "\"") (symbol "\"") anySingle

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
    [ pChar 
    , pVar
    ]

pRule :: Parser Rule
pRule = do
    var <- pVarname
    void (symbol ":")
    expr <- pExpr
    pure $ Rule var expr

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ binary "" Prod ]
    , [ binary "|" Sum ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

part1 :: Input -> Int
part1 (rules, strings) = length . filter (isJust . parseMaybe parser0) $ strings
    where rulesMap = M.fromList [(i, expr) | (Rule i expr) <- rules]
          parser0 = getParser rulesMap (rulesMap M.! 0) <* eof

part2 :: Input -> Int
part2 (rules, strings) = length . filter (isJust . parseMaybe parser0) $ strings
    where rulesMap = M.fromList [(i, expr) | (Rule i expr) <- rules]
          getParser' x = getParser rulesMap (rulesMap M.! x)
          repParser p n = try $ replicateM_ n (getParser' p)
          parser0 = do
              void $ getParser' 42
              a <- some $ getParser' 42
              void $ choice (map (repParser 31) [length a, (length a)-1..1])
              eof

prepare :: String -> Input
prepare = second (arr tail) . partitionEithers . map parse' . T.lines. T.pack
    where parse' :: Text -> Either Rule Text
          parse' t = case parseMaybe pRule t of
            Just r -> Left r
            Nothing -> Right t

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

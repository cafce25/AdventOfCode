{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative hiding (some)
import Control.Arrow ((&&&), second)
import Control.Monad
import Data.List
import Data.Maybe (fromMaybe)
import Data.Void
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Food = Food
    { ingredients :: [Ingredient]
    , allergenes :: [Allergene]
    } deriving Show
type Ingredient = String
type Allergene = String
type Input = [Food]


part1 :: Input -> Int
part1 fs = length . filter (`notElem` ai) . concatMap ingredients $ fs
    where ai = allergeneIngredients fs

allAllergenes :: [Food] -> [Allergene]
allAllergenes = foldr1 union . map allergenes

possibleIngredients :: Allergene -> [Food] -> [Ingredient]
possibleIngredients a = foldr1 intersect . map ingredients . filter (elem a . allergenes)

allergeneIngredients :: [Food] -> [Ingredient] 
allergeneIngredients = concatMap snd . allergenesToIngredients

part2 :: Input -> String
part2 = intercalate "," . map snd . sortOn fst . removeMultiples . sortOn (length . snd) . allergenesToIngredients

removeMultiples :: [(Allergene, [Ingredient])] -> [(Allergene, Ingredient)]
removeMultiples [] = []
removeMultiples ((aller, [ingr]):rest) = (aller, ingr):removeMultiples sortedRest
    where rest' = map (second (delete ingr)) rest
          sortedRest = sortOn (length . snd) rest'
removeMultiples r = error $ "ambiguos ingredients in " ++ show r

allergenesToIngredients :: [Food] -> [(Allergene, [Ingredient])]
allergenesToIngredients fs = map (id &&& (`possibleIngredients` fs)) $ allAllergenes fs

prepare :: String -> Input
prepare = fromMaybe [] . parseMaybe foodsP

foodsP :: Parser [Food]
foodsP = foodP `endBy` eol

foodP :: Parser Food
foodP = do
    ingredients <- ingredientsP
    allergenes <- parens (string "contains" *> hspace *> allergenesP) <|> pure []
    lookAhead eol
    pure Food{..}

ingredientsP :: Parser [Ingredient]
ingredientsP = some lowerChar `endBy` hspace

allergenesP :: Parser [Allergene]
allergenesP = some lowerChar `sepBy` (char ',' *> hspace)

hspace :: Parser ()
hspace = void $ some (char '\t' <|> char ' ' <|> char '\f')

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

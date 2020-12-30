{-# LANGUAGE ApplicativeDo #-}
module Main where

import           Control.Arrow ((&&&))
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.List (nub, permutations)
import           Data.Map (Map, (!))
import qualified Data.Map as M
import           Data.Void (Void)
import           System.Environment (getArgs)
import           Text.Megaparsec (Parsec, many, parseMaybe)
import           Text.Megaparsec.Char (lowerChar, string, upperChar)
import           Text.Megaparsec.Char.Lexer (decimal)

type City = String
type Distance = Int
type Edge = (City, City, Distance)
type Input = Map (City, City) Distance
type Parser = Parsec Void String

part1 :: Input -> Int
part1 input = minimum . map (tripLength input) . permutations . cities $ input

tripLength :: Map (City, City) Distance -> [City] -> Distance
tripLength _ [] = 0
tripLength _ [_] = 0
tripLength distances (f:rest@(t:_)) = tripLength distances rest + (distances!normalizedEdge f t)

cities :: Map (City, City) Distance -> [City]
cities edgeMap = nub $ fromCities ++ toCities
    where pairs = M.keys edgeMap
          fromCities = map fst pairs
          toCities = map snd pairs

part2 :: Input -> Int
part2 input = maximum . map (tripLength input) . permutations . cities $ input

edge :: Parser (City, City, Distance)
edge = do
    fromCity <- city
    void $ string " to "
    toCity <- city
    void $ string " = "
    edgeDistance <- distance
    pure (fromCity, toCity, edgeDistance)

city :: Parser City
city = (:) <$> upperChar <*> many (lowerChar <|> upperChar)

distance :: Parser Distance
distance = decimal

normalizedEdge :: City -> City -> (City, City)
normalizedEdge c1 c2 = (min c1 c2, max c1 c2)

prepare :: String -> Input
prepare input = M.fromList
    [ ((fromCity, toCity), edgeDistance)
    | line <- lines input
    , Just (city1, city2, edgeDistance) <- return . parseMaybe edge $ line
    , let (fromCity, toCity) = normalizedEdge city1 city2 
    ]

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

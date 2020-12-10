--import qualified Data.Map as Map
import Control.Applicative hiding (optional)
import Data.List
import System.Environment
import System.IO
import Text.ParserCombinators.ReadP
import Text.Read
--import qualified Data.Map as Map
import Data.Map as Map hiding (map)

newtype Color = Color String deriving (Eq, Ord)

instance Show Color where
    show (Color c) = c

instance Read Color where
    readPrec = lift $ do
        adj <- munch1 lowerCaseChar
        skipSpaces
        color <- munch1 lowerCaseChar
        return. Color. intercalate " " $ [adj, color]

newtype BagContents = BagContents (Int, Color)

instance Show BagContents where
    show (BagContents (n, color)) = show n ++ " " ++ show color ++ if n > 1 then " bags" else " bag"

instance Read BagContents where
    readPrec = lift contentBag

readBagContents :: ReadP [BagContents]
readBagContents = contentEmpty <|> (sepBy contentBag contentSep)

contentEmpty = do
    string "no other bags"
    return []

contentNonEmpty = sepBy contentBag contentSep

contentSep = do
    skipSpaces
    char ','
    skipSpaces

contentBag :: ReadP BagContents
contentBag = do
    num <- readPrec_to_P readPrec 0
    skipSpaces
    color <- readPrec_to_P readPrec 0
    skipSpaces
    string "bag"
    optional $ char 's'
    return $ BagContents (num, color)

data Bag = Bag { color :: Color
               , contents :: [BagContents] }

instance Show Bag where
    show bag = (show.color) bag ++ " bags contain " ++ cStr ++ "."
        where cStr = if length (contents bag) > 0
                    then (intercalate ", " . map show . contents) bag
                    else "no other bags"

instance Read Bag where
    readPrec = lift $ do
        color <- readPrec_to_P readPrec 0
        skipSpaces
        string "bags contain"
        skipSpaces
        contents <- readBagContents
        char '.'
        return (Bag color contents)

--instance Read BagContents where
--    readPrec = lift readBagContents


lowerCaseChar x = x >= 'a' && x <= 'z'

main = getArgs >>= parse >>= doParts . bagMapFromList . (map read :: [String] -> [Bag]) . lines

sgColor = Color "shiny gold"
doParts map = do
    putStr "Part 1: "
    print. length. bagContains sgColor$ map
    putStr "Part 2: "
    print. bagsContained sgColor $ map

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs

bagMapFromList :: [Bag] -> Map Color Bag
bagMapFromList = fromList . map (\bag -> (color bag, bag))

contentColor (BagContents c) = snd c 
contentCount (BagContents c) = fst c

bagContains :: Color -> Map Color Bag -> [Bag]
bagContains contained bags = elems.Map.filter hasShinyGolden $ bags
    where hasShinyGolden bag = if any ((== contained).contentColor) (contents bag)
              then True
              else any (hasShinyGolden.(bags!).contentColor) (contents bag)

bagsContained :: Color -> Map Color Bag -> Int
bagsContained bagColor bags = sum.map (\(BagContents (n,c)) -> n + n * bagsContained c bags) $ contents (bags!bagColor)

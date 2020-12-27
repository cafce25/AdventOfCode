{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&), second)
import Control.Monad (void)
import System.Environment (getArgs)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.List
import Data.List.Split
import Data.Maybe

type Ticket = [Int]
data Rule
    = Rule
        { fieldName :: String
        , validNumbers :: ((Int, Int), (Int, Int))
        } deriving (Show, Eq)
data Input
    = Input
        { rules :: [Rule]
        , ticket :: Ticket
        , tickets :: [Ticket]
        } deriving Show

part1 :: Input -> Int
part1 = sum . concat . uncurry offenders . (rules &&& tickets)

offenders :: [Rule] -> [Ticket] -> [Ticket]
offenders rs =  map (filter (fieldMatchesNoRule rs))

fieldMatchesARule, fieldMatchesNoRule :: [Rule] -> Int -> Bool
fieldMatchesARule rs n = any (matchRule n) rs
fieldMatchesNoRule rs = not . fieldMatchesARule rs

fieldMatchesAllRules, fieldDoesNotMatchAllRules :: [Rule] -> Int -> Bool
fieldMatchesAllRules rs n = all (matchRule n) rs
fieldDoesNotMatchAllRules rs = not . fieldMatchesAllRules rs

part2 :: Input -> Int
part2 (Input rs myTicket tix)
  = product . map ((myTicket!!) . fst)
  . filter (("departure" `isPrefixOf`) . fieldName . snd)
  $ normalized
    where tix'  = filter (all (fieldMatchesARule rs)) tix
          tix'' = filter (any (fieldDoesNotMatchAllRules rs)) tix'
          tixT  = transpose tix''
          rs'   = map (\fs -> filter (\r -> all (`matchRule` r) fs) rs) tixT
          rs''  = sortOn (length . snd) $ zip [0..] rs'
          normalized = normalize rs''
          normalize [] = []
          normalize (fr:ors)
            | length (snd fr) == 1 = (fst fr, fr'):normalize (remove fr' ors)
            | otherwise = error $ "ambiguous rules for field" ++ show fr
                where fr' = head . snd $ fr
                      remove r otherRules = sortOn (length. snd) $ map (second (\\[r])) otherRules

prepare :: String -> Input
prepare inp = Input inpRules inpTicket inpTickets
    where inpParts = splitOn "\n\n" inp
          inpRules = mapMaybe (=~parseRule) . lines . (!!0) $ inpParts
          inpTicket = (!!0) . mapMaybe (=~parseTicket) . lines . (!!1) $ inpParts
          inpTickets = mapMaybe (=~parseTicket) . lines . (!!2) $ inpParts

matchRule :: Int -> Rule -> Bool
matchRule n (Rule _ ((x1, x2), (x3, x4))) = x1 <= n && x2 >= n || x3 <= n && x4 >= n

parseRule :: RE Char Rule
parseRule = do
    name <- many $ psym (/= ':')
    void $ string ": "
    x1 <- decimal
    void $ sym '-'
    x2 <- decimal
    void $ string " or "
    y1 <- decimal
    void $ sym '-'
    y2 <- decimal
    pure $ Rule name ((x1, x2), (y1, y2))

parseTicket :: RE Char Ticket
parseTicket = do
    nums <- many num 
    string ""
    pure nums 

num :: RE Char Int
num = decimal <* many (sym ',')

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

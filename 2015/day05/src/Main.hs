{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Foldable (asum)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import Text.Regex.Applicative ( anySym
                              , many
                              , match
                              , psym
                              , string )
import Text.Regex.PCRE hiding (match)

type Input = [String]

niceString :: String -> Bool
niceString s = all (\x -> x s) [ containsNOf 3 "aeiou"
                               , containsTuple
                               , not. containsNaughtySubstring ["ab", "cd", "pq", "xy"]]

containsNOf :: Int -> [Char] -> String -> Bool
containsNOf n chars = isJust . match (many anySym *> replicateM n (psym (`elem` chars) *> many anySym))

containsTuple :: String -> Bool
containsTuple (x:xs@(y:_))
    | x == y = True
    | otherwise = containsTuple xs
containsTuple _ = False

containsNaughtySubstring :: [String] -> String -> Bool
containsNaughtySubstring naughty = isJust . match (many anySym *> asum (map string naughty) *> many anySym)

part1 :: Input -> Int
part1 = length . filter niceString

newNiceString :: String -> Bool
newNiceString s = all (\x -> x s) [ (=~ "(..).*\\1")
                                  , (=~ "(.).\\1")]

part2 :: Input -> Int
part2 = length. filter newNiceString

prepare :: String -> Input
prepare = lines

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

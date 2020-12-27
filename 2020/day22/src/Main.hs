module Main where

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)

type Deck = Seq Int
type Input = (Deck, Deck)
type State = Set (Deck, Deck)

battle :: Deck -> Deck -> (Deck, Deck)
battle a@S.Empty b = (a, b)
battle a b@S.Empty = (a, b)
battle (a :<| as) (b :<| bs) = if a > b then (as |> a |> b, bs)
                                        else (as, bs |> b |> a)

battleOut :: Deck -> Deck -> Deck
battleOut S.Empty b = b
battleOut a S.Empty = a
battleOut a b = uncurry battleOut $ battle a b 

part1 :: Input -> Int
part1 = sum . zipWith (*) [1..] . toList . S.reverse . uncurry battleOut 

battle' :: Deck -> Deck -> (Deck, Deck)
battle' a@S.Empty b = (a, b)
battle' a b@S.Empty = (a, b)
battle' (a :<| as) (b :<| bs)
    | length as >= a && length bs >= b
    = if winA Set.empty (S.take a as) (S.take b bs)
         then resultA
         else resultB
    | a > b = resultA
    | otherwise = resultB
    where resultA = (as |> a |> b, bs)
          resultB = (as, bs |> b |> a)

battleOut' :: State -> Deck -> Deck -> (State, Deck, Deck)
battleOut' s S.Empty b = (Set.insert (S.Empty, b) s, S.Empty, b)
battleOut' s a S.Empty = (Set.insert (a, S.Empty) s, a, S.Empty)
battleOut' s a b = if (a, b) `Set.member` s
                       then (s, a, S.Empty)
                       else uncurry (battleOut' (Set.insert (a,b) s)) $ battle' a b

winA :: State -> Deck -> Deck -> Bool
winA s a b = case battleOut' s a b of
               (_, _, S.Empty) -> True
               _               -> False

part2 :: Input -> Int
part2 = sum . zipWith (*) [1..] . toList . S.reverse . winner
    where winner decks = case uncurry (battleOut' Set.empty) decks of
                           (_, S.Empty, b) -> b
                           (_, a, _) -> a

prepare :: String -> Input
prepare = (head &&& head. tail) . map (S.fromList . map read . tail . lines) . splitOn "\n\n"

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

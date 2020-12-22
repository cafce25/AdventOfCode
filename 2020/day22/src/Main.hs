module Main where

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Sequence (Seq((:<|)), (|>))
import qualified Data.Sequence as S
import System.Environment (getArgs)

type Deck = Seq Int
type Input = (Deck, Deck)

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

part2 :: Input -> ()
part2 = const ()

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

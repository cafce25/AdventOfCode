{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow ((&&&))
import Data.List
import Data.Map.Lazy (Map, (!))
import Data.Maybe (fromJust)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec hiding (getInput)   
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Lazy as M


type Input = Tree
type Parser = Parsec Void String
data TreeBuilder = TreeBuilder 
    { bname :: String 
    , bweight :: Int 
    , bchildren :: [String]
    } deriving Show
data Tree = Tree
    { name :: String
    , weight :: Int
    , children :: [Tree] 
    }
      | Leaf 
    { name :: String 
    , weight :: Int
    } deriving Show

part1 :: Input -> String
part1 = name

calculateWeight :: Tree -> Int
calculateWeight (Leaf _ w) = w
calculateWeight (Tree _ w cs) = w + (sum $ map calculateWeight cs)

findCorrectedWeight :: Int -> Tree -> Int
findCorrectedWeight sw (Leaf _ _) = sw
findCorrectedWeight sw (Tree _ _ cs) = if length gws > 1
                                         then findCorrectedWeight rWeight wTree
                                         else sw - (sum $ map calculateWeight cs)
    where cws = sort $ map calculateWeight cs
          gws = group cws
          wTree = fromJust $ find ((wWeight ==) . calculateWeight) cs
          wWeight = head $ fromJust $ find ((1 ==) . length) gws
          rWeight = head $ fromJust $ find ((1 /=) . length) gws

part2 :: Input -> Int
part2 t = findCorrectedWeight (calculateWeight t) t 

getRoot :: Map String Tree -> Tree
getRoot = head . reverse . sortOn depth . M.elems

depth :: Tree -> Int
depth (Leaf _ _) = 1
depth (Tree _ _ cs) = 1 + (maximum . map depth $ cs)

treeP :: Parser TreeBuilder
treeP = do
    bname <- some lowerChar 
    space
    bweight <- between (char '(') (char ')') L.decimal
    space
    bchildren <- string "-> " *> some lowerChar `sepBy` string ", " <|> pure []
    pure TreeBuilder{..}

prepare :: String -> Input
prepare inp = getRoot treeMap
    where treeMap = M.fromList
              [(k, v)| v' <- l
                , let vb = fromJust $ parseMaybe treeP v'
                , let k = bname vb
                , let v = makeTree vb]
          l = lines inp
          makeTree (TreeBuilder n w []) = Leaf n w
          makeTree (TreeBuilder n w cs) = Tree n w (map (treeMap!) cs)
          

main :: IO ()
main = getInput >>= print . (part1 &&& part2) . prepare

getInput :: IO String
getInput = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

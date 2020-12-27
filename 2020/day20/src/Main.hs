{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.List
import Data.Maybe (fromMaybe)
import Data.Void
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Input = [Tile]
data Tile = Tile
    { tileId :: Int
    , tileData :: [String]
    } deriving Show

applyT :: ([String] -> [String]) -> (Tile -> Tile)
applyT f tile@Tile{ tileData = d } = tile{ tileData = f d }

flipV, flipH, transposeT, rotateR, rotateL, rotate180, transposeT' :: Tile -> Tile
flipV       = applyT  reverse
transposeT  = applyT  transpose
flipH       = applyT (map reverse)
rotateR     = applyT (map reverse . transpose)
rotateL     = applyT (reverse . transpose)
rotate180   = applyT (map reverse . reverse)
transposeT' = applyT (reverse . transpose . map reverse)


edges :: Tile -> [String]
edges Tile{ tileData = d } = [ head d, last d, head d', last d' ]
    where d' = transpose d

haveCommonEdge :: Tile -> Tile -> Bool
haveCommonEdge a b = any (uncurry (||) . (inBEdges &&& (inBEdges . reverse))) (edges a)
    where inBEdges = (`elem` edges b)

part1 :: Input -> Int --Map String Tile
part1 inp = product . map tileId . filter ((==2) . countCommonEdges) $ inp
    where countCommonEdges e = pred . length . filter (haveCommonEdge e) $ inp

part2 :: Input -> ()
part2 = const ()

pId :: Parser Int
pId = do
    void $ string "Tile "
    tId <- L.decimal
    void $ string ":\n"
    pure tId

pTile :: Parser Tile
pTile = do
    tileId <- pId
    tileData <- replicateM 10 (replicateM 10 (char '.' <|> char '#') <* char '\n')
    pure $ Tile{..}

pTiles :: Parser [Tile]
pTiles = (pTile `endBy` string "\n") <* eof
    
prepare :: String -> Input
prepare inp = tiles
    where mTiles = parseMaybe pTiles inp
          tiles = fromMaybe [] mTiles

main :: IO ()
main = getInputContent >>= print . (part1 &&& part2) . prepare

getInputContent :: IO String
getInputContent = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

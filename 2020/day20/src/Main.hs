{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Data.Ix (range)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Input = [(Id, Tile)]
type Id = Int
newtype Tile = Tile [String]
type Edge = String
type Coord = (Int, Int)

flipV, flipH, transposeT, rotateR, rotateL, rotate180, transposeT' :: Tile -> Tile
flipV       (Tile a) = Tile . reverse                       $ a
transposeT  (Tile a) = Tile . transpose                     $ a
flipH       (Tile a) = Tile . map reverse                   $ a
rotateR     (Tile a) = Tile . map reverse . transpose       $ a
rotateL     (Tile a) = Tile . reverse . transpose           $ a
rotate180   (Tile a) = Tile . map reverse . reverse         $ a
transposeT' (Tile a) = Tile . reverse . transpose . reverse $ a

tData :: Tile -> [String]
tData (Tile a) = a

instance Eq Tile where
    a == (Tile b)
      =  b `elem` sequence [ tData
                           , tData . flipV
                           , tData . flipH
                           , tData . rotateL
                           , tData . rotateR
                           , tData . transposeT
                           , tData . transposeT'
                           , tData . rotate180
                           ] a 

instance Show Tile where
    show (Tile a) = "\n" ++ unlines a

edges :: Tile -> (Edge, Edge, Edge, Edge)
edges (Tile d0) = edges'
    where edges' = case (d0, reverse d0, transpose d0, reverse $ transpose d0) of
                     (t:_, b:_, l:_, r:_) -> (t, b, l, r)
                     _ -> error "edges: tile is empty"

edgesList :: Tile -> [Edge]
edgesList tile = [t, b, l, r]
    where  (t, b, l, r) = edges tile

isAdjacent :: Tile -> Tile -> Bool
isAdjacent t = any (`isEdge` t) . edgesList

isEdge :: Edge -> Tile -> Bool
e `isEdge` t = e `elem` aEdges || reverse e `elem` aEdges
    where aEdges = edgesList t

part1 :: Input -> Int
part1 inp = product . map fst . filter ((`elem` cornerTiles (map snd inp)) . snd) $ inp

(<.>) :: Tile -> Tile -> Tile
ta <.> tb 
  | rightA == leftB = Tile $ zipWith (\a b -> init a ++ tail b) (tData ta) (tData tb)
  | otherwise = error "<.>: joining on unequal edges"
    where (_, _, _, rightA) = edges ta
          (_, _, leftB, _) = edges tb

(</>) :: Tile -> Tile -> Tile
a </> b 
  | topB == botA = Tile $ init (tData a) ++ tail (tData b)
  | otherwise = error "</>: joining on unequal edges"
    where (_, botA, _, _) = edges a
          (topB, _, _, _) = edges b

assemble :: [[Tile]] -> Tile
assemble = Tile . map (init.tail) . init . tail . tData . foldl1 (</>) . map (foldl1 (<.>))

middle :: [a] -> Maybe [a]
middle l = case length l of
             0 -> Nothing
             1 -> Nothing
             _ -> Just . init . tail $ l

arrange :: [Tile] -> [[Tile]]
arrange tiles
  = reverse . transpose
  . map (\x -> arrangeColumn x (cs' ++ es ++ cens))
  $ firstRow
    where firstTile = getOrientedFirst tiles
          (cs, es, cens) = splitTiles tiles
          cs' = tail cs
          firstRow = arrangeRow firstTile (cs' ++ es)

splitTiles :: [Tile] -> ([Tile], [Tile], [Tile])
splitTiles tiles = (cs, es, centrals)
    where cs = cornerTiles tiles
          es = edgeTiles tiles
          centrals = tiles \\ (cs ++ es)

monster :: Tile
monster = Tile [ "                  # "
               , "#    ##    ##    ###"
               , " #  #  #  #  #  #   "
               ]

allMonsters :: [Tile]
allMonsters = sequence [ id 
                       , flipH
                       , flipV
                       , rotate180
                       , rotateL
                       , transposeT'
                       , transposeT
                       , rotateR
                       ] monster
toSet :: Tile -> Set Coord
toSet (Tile t) = S.fromList [(x, y) | (y, l) <- zip [0..] t, (x, p) <- zip [0..] l, p == '#']

shiftCoordSet :: Coord -> Set Coord -> Set Coord
shiftCoordSet (dx, dy) = S.map (\(x, y) -> (x + dx, y + dy))

cornerTiles, edgeTiles :: [Tile] -> [Tile]
cornerTiles ts = filter (\t -> 2 == countAdjacent (delete t ts) t) ts
edgeTiles   ts = filter (\t -> 3 == countAdjacent (delete t ts) t) ts

countAdjacent :: [Tile] -> Tile -> Int
countAdjacent ts t = length . filter (isAdjacent t) $ ts

countWithEdge :: [Tile] -> Edge -> Int
countWithEdge ts e = length . filter (isEdge e) $ ts

orientBottom, orientLeft :: Edge -> Tile -> Tile
orientLeft   edge = rotateR . orientBottom edge
orientBottom edge tile
  | edge  == bottom =             tile
  | edge' == bottom = flipH       tile
  | edge  == top    = flipV       tile
  | edge' == top    = rotate180   tile
  | edge  == left   = rotateL     tile
  | edge' == left   = transposeT' tile
  | edge  == right  = transposeT  tile
  | edge' == right  = rotateR     tile
  | otherwise = error
              $ "could not match \"" ++ edge
              ++ "\" to any edge of tile \n============\n"
              ++ unlines (tData tile)
    where edge' = reverse edge
          (top, bottom, left, right) = edges tile

arrangeColumn, arrangeRow :: Tile -> [Tile] -> [Tile]
arrangeRow pt = map transposeT' . arrangeColumn (transposeT' pt)
arrangeColumn pt ts
  | null (tData fittingTile) = [pt]
  | otherwise =
      let oriented = orientBottom prevTop fittingTile
       in pt : arrangeColumn oriented ts'
      where fittingTile = fromMaybe (Tile []) $ find (isEdge prevTop) ts'
            (prevTop, _, _, _) = edges pt
            ts' = delete pt ts

getOrientedFirst :: [Tile] -> Tile
getOrientedFirst tiles = getOrientedFirst' aCorner (delete aCorner tiles)
    where aCorner = case cornerTiles tiles of
                      (h:_) -> h
                      _ -> error "getOrientedFirst: 'cornerTiles tiles' is empty"
          getOrientedFirst' :: Tile -> [Tile] -> Tile
          getOrientedFirst' t ts
              | 1 == (ts `countWithEdge` topEdge)
              = if 1 == (ts `countWithEdge` leftEdge)
                   then flipH t
                   else t
              | otherwise =  getOrientedFirst' (rotateR t) ts
              where (topEdge, _, leftEdge, _) = edges t

countMonsters :: Tile -> Tile -> Int
countMonsters m t = length . filter monsterAt $ range ((0, 0), (wt - wm, ht - hm)) 
    where wt = length . head . tData $ t
          ht = length . tData $ t
          wm = length . head . tData $ m
          hm = length . tData $ m
          ms = toSet m
          ts = toSet t
          monsterAt :: Coord -> Bool
          monsterAt pos = shiftCoordSet pos ms `S.isProperSubsetOf` ts

part2 :: Input -> Int
part2 inp = tileSize - monsterSize * monsters
    where monsters = maximum $ mapM countMonsters allMonsters fullTile
          fullTile = assemble . arrange . map snd $ inp
          tileSize = S.size $ toSet fullTile
          monsterSize = S.size $ toSet monster

pId :: Parser Int
pId = between (string "Tile ") (string ":" <* eol) L.decimal

pTile :: Parser (Id, Tile)
pTile = (\a b -> (a , Tile b)) <$> pId <*> replicateM 10 (replicateM 10 (char '.' <|> char '#') <* char '\n')

pTiles :: Parser [(Id, Tile)]
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

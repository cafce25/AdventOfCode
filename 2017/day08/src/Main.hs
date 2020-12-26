{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Parser = Parsec Void String
type Input = [Instr]
type Reg = String
type Val = Int
data Con = Gt Reg Val 
         | Lt Reg Val 
         | Ge Reg Val
         | Le Reg Val
         | E Reg Val
         | Ne Reg Val
         deriving Show
data Instr = Inc Reg Val Con
           | Dec Reg Val Con
           deriving Show


run :: [Instr] -> (Map Reg Val, Int)
run = foldl step (M.empty, 0)
    where step (memory, h) instr =
             let (reg, change, cond) =
                     case instr of
                       Inc r v c -> (r, v, c)
                       Dec r v c -> (r, -v, c)
                 (cReg, cmp, cVal) =
                     case cond of
                       Gt r v -> (r, (>), v)
                       Lt r v -> (r, (<), v)
                       Ge r v -> (r, (>=), v)
                       Le r v -> (r, (<=), v)
                       E  r v -> (r, (==), v)
                       Ne r v -> (r, (/=), v)
                 pval = M.findWithDefault 0 reg memory
                 val = pval + change
              in if M.findWithDefault 0 cReg memory `cmp` cVal
                    then (M.insert reg val memory, max h val)
                    else (memory, h)

part1 :: Input -> Int
part1 = maximum . M.elems . fst . run

part2 :: Input -> Int
part2 = snd . run

insP :: Parser Instr
insP = try decP <|> incP
    
decP :: Parser Instr
decP = insP' "dec" Dec

incP :: Parser Instr
incP = insP' "inc" Inc

insP' :: String -> (Reg -> Val -> Con -> Instr) -> Parser Instr
insP' op constr = do
    reg <- some lowerChar
    space
    string op
    space
    val <- signed space decimal
    space
    con <- conP
    pure (constr reg val con)

conP :: Parser Con
conP = do
    string "if"
    space
    reg <- some lowerChar
    space
    comp <- choice [ Le <$ string "<="
                   , Lt <$ string "<"
                   , E  <$ string "=="
                   , Ne <$ string "!="
                   , Ge <$ string ">="
                   , Gt <$ string ">"
                   ]
    space
    val <- signed space decimal
    pure (comp reg val)

inpP :: Parser Input
inpP = insP `endBy` eol

prepare :: String -> Input
prepare = fromJust . parseMaybe inpP

main :: IO ()
main = getInputContents >>= print . (part1 &&& part2) . prepare

getInputContents :: IO String
getInputContents = do
    args <- getArgs
    case args of
        []    -> readFile "input"
        ["-"] -> getContents
        fs    -> concat `fmap` mapM readFile fs

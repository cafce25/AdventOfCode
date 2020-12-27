import System.Environment
data Instruction = Nop | Acc Int | Jmp Int deriving (Eq, Show, Ord)

main = getArgs >>= parse >>= print . findLoop [] 0 0 . map parseOp . lines

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs

parseOp :: String -> Instruction
parseOp line = case opStr of
    "nop" -> Nop
    "acc" -> Acc (parseArg argStr)
    "jmp" -> Jmp (parseArg argStr)
    where
        [opStr, argStr] = words line
        parseArg ('+':argStr) = read argStr
        parseArg ('-':argStr) = -read argStr
        parseArg argStr = read argStr



findLoop :: [Int] -> Int -> Int-> [Instruction] -> Int
findLoop past line acc instructions = if line `elem` past
    then acc
    else case instructions !! line of
        Nop ->     findLoop (line:past) (line + 1)    acc        instructions
        Jmp num -> findLoop (line:past) (line + num)  acc        instructions
        Acc num -> findLoop (line:past) (line + 1)   (acc + num) instructions

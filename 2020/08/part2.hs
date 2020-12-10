import System.Environment
data Instruction = Nop Int | Acc Int | Jmp Int deriving (Eq, Show, Ord)
data End = Loop Int | End Int deriving (Show)

main = getArgs >>= parse >>= print . filter isEnd . (\x -> map (\y -> findLoopOrExit [] 0 0 y x) [0..(length x)])  . map parseOp . lines

parse [] = readFile "input"
parse fs = concat `fmap` mapM readFile fs

isEnd :: End -> Bool
isEnd (End _) = True
isEnd _ = False

parseOp :: String -> Instruction
parseOp line = case opStr of
    "nop" -> Nop (parseArg argStr)
    "acc" -> Acc (parseArg argStr)
    "jmp" -> Jmp (parseArg argStr)
    where
        [opStr, argStr] = words line
        parseArg ('+':argStr) = read argStr
        parseArg ('-':argStr) = (- read argStr)
        parseArg argStr = read argStr


findLoopOrExit :: [Int] -> Int -> Int -> Int -> [Instruction] -> End
findLoopOrExit past line acc swap instructions
    | line == length instructions = End acc
    | line `elem` past = Loop acc
    | otherwise = case instructions !! line of
        Nop num -> if line /= swap then nop num else jmp num
        Jmp num -> if line /= swap then jmp num else nop num
        Acc num -> accf num
        where
            nop num  = findLoopOrExit (line:past) (line + 1)    acc        swap instructions
            jmp num  = findLoopOrExit (line:past) (line + num)  acc        swap instructions
            accf num = findLoopOrExit (line:past) (line + 1)   (acc + num) swap instructions

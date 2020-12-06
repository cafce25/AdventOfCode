import Data.List
main = interact $ show . maximum . map seatToNumber . lines -- part One

seatToNumber :: String -> Int
seatToNumber seat = seatToNumber' seat 0
    where
        seatToNumber' [] num = num
        seatToNumber' (x:seat) num = seatToNumber' seat (2 * num + charToBit x)

charToBit :: Char -> Int
charToBit 'B' = 1
charToBit 'R' = 1
charToBit _ = 0

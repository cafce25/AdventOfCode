import Data.List
main = interact
     $ show . (\x -> x-1) . fst . head
     . dropWhile (uncurry (>)) . zip [97..]
     . sort . map seatToNumber . lines

seatToNumber :: String -> Int
seatToNumber seat = seatToNumber' seat 0
    where
        seatToNumber' [] num = num
        seatToNumber' (x:seat) num = seatToNumber' seat (2 * num + charToBit x)

charToBit :: Char -> Int
charToBit 'B' = 1
charToBit 'R' = 1
charToBit _ = 0

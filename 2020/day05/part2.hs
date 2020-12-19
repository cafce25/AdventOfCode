import Data.List
--main = interact $ show . maximum . map seatToNumber . lines -- part One
main = interact $ show .
    (\x -> x-1) . fst . head . snd . -- extract seat after mine, -1 = my seat
    span (\(x, y) -> x > y) . zip [97..] . sort . -- split into seats before and after mine
    map seatToNumber . lines -- get all other seats

seatToNumber :: String -> Int
seatToNumber seat = seatToNumber' seat 0
    where
        seatToNumber' [] num = num
        seatToNumber' (x:seat) num = seatToNumber' seat (2 * num + charToBit x)

charToBit :: Char -> Int
charToBit 'B' = 1
charToBit 'R' = 1
charToBit _ = 0

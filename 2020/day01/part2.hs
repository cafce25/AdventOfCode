module Main where

main = interact $ show . prodSumEq3 2020 . map read . lines


prodSumEq :: (Num a, Eq a) => a -> [a] -> a
prodSumEq x ys = uncurry (*) . head . filterSumIsX x $ crossSquare ys
    where
        filterSumIsX x = filter (\(a, b) -> x == a + b)
        crossSquare xs = [(x, y) | x <- xs, y <- xs]

prodSumEq3 :: (Num a, Eq a) => a -> [a] -> a
prodSumEq3 x ys = (\(x, y, z) -> x * y * z) . head . filterSumIsX x $ crossCube ys
    where
        filterSumIsX x = filter (\(a, b, c) -> x == a + b + c)
        crossCube xs = [(x, y, z) | x <- xs, y <- xs, z <- xs]

main = interact $ show . basementWhen

basementWhen = basementWhen' 0 0
    where
        basementWhen' depth _ [] = depth
        basementWhen' depth (-1) _ = depth
        basementWhen' depth floor ('(':xs) = basementWhen' (depth+1) (floor+1) xs
        basementWhen' depth floor (')':xs) = basementWhen' (depth+1) (floor-1) xs

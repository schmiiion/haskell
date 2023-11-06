isPrime3 :: Int -> Bool
isPrime3 n
    | n <= 1 = False
    | n <= 3 = True
    | even n || n `mod` 3 == 0 = False
    | otherwise = isPrime' n 5

isPrime' :: Int -> Int -> Bool
isPrime' n i
    | i * i > n = True
    | n `mod` i == 0 || n `mod` (i + 2) == 0 = False
    | otherwise = isPrime' n (i + 6)
import Distribution.Simple.Command (multiOption)
-- Tuples
type Cash = (Int, Int)

addNumbers :: (Int, Int) -> (Int, Int) -> (Int, Int)
addNumbers (a, b) (c, d) 
                | b + d >= 100  = (a+c+1, mod (b+d) 100)
                | otherwise     = (a+c, b+d)

addNumers' :: (Int, Int) -> (Int, Int) -> (Int, Int)
addNumers' (x,x') (y,y') = (x + y + div z 100, mod z 100)
                            where z = x' + y' 

addNumers'' :: Cash -> Cash -> Cash
addNumers'' (x,x') (y,y') = (x + y + div z 100, mod z 100)
                            where z = x' + y' 

type Name = String
type Marks = (Int, Int, Int)
type Entry = (Name, Marks)
type Total = (Name, Int)

getMarks :: Entry -> Total
getMarks (name, (a, b, c)) = (name, result)
    where result = a + b + c


--LISTS
-- head and tail break up the list from the front
-- last and init break up the list from the back
-- take and drop split a list at the nth element

customTake :: Int ->[a] -> [a]
customTake n [] = []
customTake 0 _ = []
customTake n (x:xs) = x : customTake (n-1) xs

-- using last and init
lastTwo :: [a] -> [a]
lastTwo list = [last (init list) , last list]

-- using drop and length
lastTwo' :: [a] -> [a]
lastTwo' list = drop (length list -2) list


replicate' :: Int -> a -> [a]
replicate' n a = [a | k <- [1..n]]

isPrime :: Int -> Bool
isPrime n = not (isPrimeHelper n (div n 2))

isPrimeHelper :: Int -> Int -> Bool
isPrimeHelper m n 
            | n < 2 = False
            | otherwise = isPrimeHelper m (n-1) || (mod m n == 0)


factors :: Int -> [Int]
factors n = [k | k <- [1..n], mod n k == 0 ]

multiples :: Int -> Int -> [Int]
multiples x y = [k | k <- [1..100], mod k x == 0 && mod k y == 0]

-- only for vectors of even length
skalarProd :: Num a => [a] -> [a] -> a
skalarProd xs ys = sum [x * y | (x, y ) <- zip xs ys]


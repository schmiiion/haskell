--1
double :: Integer -> Integer
double input = input * 2

--2
ratio :: Float -> Float -> Float
ratio x y = (x + y) / (x - y) 

--3
hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt(a ** 2 + b ** 2)

--4
xIntercept :: Double -> Double -> Double
xIntercept 0 0 = 0
xIntercept 0 _ = error "won't intercept x-axis you idiot"
xIntercept m c = (-c) / m

--5
threeDiff :: Integer -> Integer -> Integer -> Bool
threeDiff m n p = (m /= n) && (n /= p) && (p /= m)

--6
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral (x + y + z) / 3 

--7
arithmeticSum :: Int -> Int -> Int -> Int
arithmeticSum a n d =  div (n * (2 * a + (n - 1) * d))  2

--8
inrange1 :: (Ord a) => a -> a -> a -> Bool
inrange1 x a b = ((x > a)&&(x < b)) || ((x > b)&&(x < a))

--9 
orExclusive :: Bool -> Bool -> Bool
orExclusive x y = (x && not y) || (not x && y)

orExclusive' :: Bool -> Bool -> Bool
orExclusive' = (/=)

--10
implies :: Bool -> Bool -> Bool
implies a b = not a || b

--11
hundreds :: Int -> Int
hundreds a = mod (div a 100) 10

--12
middle :: String -> Char
middle a    
        | len == 0 = error "String is empty"
        | len == 1 = head a
        | len == 2 = head (tail a)
        | otherwise = middle (tail (init a))
    where 
        len = length a 



---------RECURSIVE

--1
natural :: Int -> Int
natural a 
        | a <= 1 = 0
        | otherwise =  1 + natural (a - 1)

--2
odds :: Int -> Int
odds a 
        | a <= 0 = 0
        | a == 1 = 1
        | otherwise = 2 + odds (a - 1)

--3
sumRec :: Int -> Int 
sumRec a
        | a < 1 = 0
        | otherwise = a + sumRec (a - 1)

--4
fact :: Int -> Int 
fact 0 = 1
fact n = n * fact (n - 1)

--5
sumFact :: Int -> Int 
sumFact 0 = 1
sumFact n = fact n + sumFact (n - 1)

sumFact' :: Int -> Int
sumFact' n
        | n < 0 = 0
        | otherwise = fact n + sumFact (n - 1)

--6
arithmeticSeries :: Int -> Int -> Int -> Int
arithmeticSeries a n d
        | n < 1 = a -- TODO shouldnt it be less than 0
        | otherwise = d + arithmeticSeries a (n - 1) d 


--7 
natural' :: Int -> Int 
natural' n = arithmeticSeries 0 (n - 1) 1

odds' :: Int -> Int
odds' n = arithmeticSeries 1 (n - 1) 2 


--8
arithmeticSumRec :: Int -> Int -> Int -> Int
arithmeticSumRec a n d
        | n <= 1 = a -- check basecase
        | otherwise = a + (n - 1) * d + (arithmeticSumRec a (n - 1) d)


--9
multRecInt :: Int -> Int -> Int 
multRecInt m n 
        | n == 0 || m == 0 = 0
        | n < 0 = if m < 0
                    then multRecInt ((-1) * m) ((-1) * n)
                    else (-1) * multRecInt m ((-1) * n)
        | m < 0 = multRecInt ((-1) * m) ((-1) * n)
        | n == 1 = m 
        | otherwise = multRecInt m (n - 1) + m


--10
rangeProduct1 :: Int -> Int -> Int 
rangeProduct1 m n 
        | n < m = 1
        | m == n = n
        | otherwise = multRecInt m (rangeProduct1 (m + 1) n)

rangeProduct2 :: Int -> Int -> Int
rangeProduct2 m n 
        | n < m = 1
        | m == n = m 
        | otherwise = multRecInt (rangeProduct2 m (n - 1))  n


--11
intSqrt :: Int -> Int
intSqrt n 
        | n < 0 = 0
        | otherwise = intSqrt' n n 


intSqrt' :: Int -> Int -> Int 
intSqrt' n m 
        | (m * m) < n = m 
        | otherwise = intSqrt' n (m - 1)


--12
fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo(n - 1) + fibo(n - 2)

superFibo :: Int -> Int
superFibo _ = 0

-- fiboTwo n == (fibo n, fibo (n + 1))
fiboTwo :: Int -> (Int, Int)
fiboTwo 0 = (1,1)
fiboTwo 1 = (1,2)
fiboTwo n = step (fiboTwo (n-1))

step :: (Int, Int) -> (Int, Int)
step (x, y) = (y, x+y)

-- Exercise 5.1 : LISTS
listEvents :: Int -> Int -> [Int]
listEvents x y = [n | n <- [y, y-1..x], even n ]


type Triple = (Int, Int ,Int)
pythagoreanTriples :: Int -> [Triple]
pythagoreanTriples n = [(a, b ,c ) |    c <- [1..n],
                                        b <- [1..c],
                                        a <- [1..b], a^2 + b^2 == c^2]


addPairwise :: Num a => [a] -> [a] -> [a]
addPairwise (x:xs) (y:ys) = x + y : addPairwise xs ys
addPairwise _ _ = []


addPairwise' :: Num a => [a] -> [a] -> [a]
addPairwise' xs ys = zipWith (+) xs ys


subList :: [a] -> (Int, Int) -> [a]
subList xs (a, b) = [xs !! n | n <- [a..b]]

subList' :: [a] -> (Int, Int) -> [a]
subList' xs (a, b) = take (b-a+1)  $ drop a xs

injectList :: [a] -> [a]    
injectList xs = [x | x <- xs]

together :: [a] -> [(a,a)]
together [_] = []
together (x:xs) = (x, head xs) : together xs


-- Exercise 5.2 : RECURSION OVER LISTS
contains :: Eq a => [a] -> a -> Bool
contains [] elem = False
contains (x:xs) elem = x == elem || contains xs elem

contains' :: Eq a => [a] -> a -> Bool
contains' (x: xs) = foldr 
-- HIGHER ORDER FUNCTIONS

-- on Lists: all, any
-- A predicate is a function that returns a boolean 
-- all :: (a -> Bool) -> [a] -> Bool 
-- all p xs = isSmall 


isSmall :: Char -> Bool
isSmall c = c >= 'a' && c <= 'z'

allSmall :: [Char] -> Bool
allSmall = all isSmall 

anySmall :: [Char] -> Bool
anySmall = any isSmall

-- MAPPING
incrementAllCumb :: Num a => [a] -> [a]
incrementAllCumb [] = []
incrementAllCumb (x:xs) = x+1 : incrementAllCumb xs

incrementAll :: Num a => [a] -> [a]
incrementAll xs = map ( + 1) xs

allLessThan :: Int -> [Int] -> Bool
allLessThan n = all (< n)   

singleton :: [a] -> [[a]]
singleton xs = map (: []) xs

squareOdds :: [Int] -> [Int]
squareOdds xs = map (^2) (filter odd xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [a] = a
foldr1' f (x:xs) = f x (foldr1' f xs)
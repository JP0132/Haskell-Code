module Worksheet4A where
import Data.Char

--Q1
--a) 
--i} [(Int,Int)] -> [(Int, Int)] -> [(Int,Int),(Int,Int)] list of typle integers
--ii) Char -> [Char] -> [Char] list of character / string
--iii) (Int, (Int, Int, Int)) tuples of integer
--iv) ((a -> a -> Bool) Float) funtion float > Bool
--v) [[Char] | [Char] <- [Char]] list of characters / string

--b) 
--i) glap :: Ord a => [a] -> [Int]
--ii) glup :: Ord a => [a] -> [Char]
--iii) glop :: Ord a => [a] -> [Char]

--c)
--i} 2
--ii) 5 
--iii) [False]
--iv) [10,8,11,9,12,10]

--d)
--i) filter :: (a -> Bool) -> [a] -> [a]
--ii)   filter ::  (a -> Bool) -> [a] -> [a]
 --     filter f [] = []
 --     filter f (x:xs)
 --         | f x  = x : filter(f xs)
 --         | otherwise = filter(f xs)

--iii)  filter :: (a -> Bool) -> [a] -> [a]
--      filter f xs = [x | x <- xs, f x]

--e)
at :: Ord a => [a] -> Integer -> a
at [] n = error "Empty"
at list n = at' list n 1

at' :: Ord a => [a] -> Integer -> Integer -> a
at' [] n i = error "Index out of range"
at' (x:xs) n i
    | n == i = x
    |otherwise = at' xs n (i+1) 

--f)
countDif ::  Ord a => [a] -> Integer
countDif [] = 0
countDif list = countDif' list 0

check :: (Eq a) => [a] -> a -> Bool
check [] _ = False
check (x:xs) a
    | x == a = True
    | otherwise = check xs a


countDif' ::  Ord a => [a] -> Integer -> Integer
countDif' [] i = i
countDif' (x:xs) i
    |check xs x = countDif' xs i
    |otherwise = countDif' xs (i+1)


pair :: a -> (a,a)
pair x = (x,x)

-- fun :: Integer -> Integer -> Integer
-- fun f z (x:xs) = f x (fun f z xs)


-- listIsEqual :: [[Int]] -> Bool
-- listIsEqual myList = all (\x -> length x == length myList) myList

rotateAll :: [a] -> [[a]]
rotateAll xs = rotateAll' xs (length xs)
    where rotateAll' xs 1 = [xs]
          rotateAll' xs n = xs : rotateAll' (rotate xs) (n - 1)


-- rotate :: Int -> [[a]] -> [[a]]
-- rotate _ [] = []
-- rotate n xs = zipWith const (drop n (cycle xs)) xs


rl :: [[Int]] -> [[Int]]
rl [] = []
rl ([]:_) = []
rl m = map last m : (rl (map init m))

-- type Triangle = [[Int]]
-- rotateA :: Triangle -> Triangle
-- rotateA = reverse . transpose

module ExamP where 

import Data.Ord
import Data.Char
import Data.List

--EXAM PAPER 2018 
--(a) What are the most general types of the following terms:  
-- i.   [(2,0)]++[(1,8)] =       [(Int,Int)]
-- ii.  ’M’:"ay" =               [Char]
-- iii. (2,(0,1,8)) =            (Int,(Int,Int,Int))
-- iv.  (<= 20.18) =             Bool 
--                               CORRECT ANS: Float -> Bool
-- v.   [ x |x<-"2018"] =        [Char]

-- (b) Give the most general type declarations for the following function definitions:
-- i. glap (a:as) = 1 : glap as
-- glap :: [Int] -> [Int]
-- ii. glup (a:as) = ’1’ : glup as
-- glup :: [Char] -> [Char]
-- iii. glop (a:as) = "1": glop as 
-- glop :: [String] -> [String]

--  Evaluate:
-- i. fst(2,0)*head[1,8] = 2*head[1,8] = 2*1 = 2
-- ii. 5 + head(tail( [2,0]++[1,8])) = 5 + head(tail([2,0,1,8])) = 5 + head([0,1,8]) = 5 + 0 = 5
-- iii. filter not [True,False] = [False]
-- iv. [x-y | x<-[14..16], y<-[4,6]] = [10, 8] = [10, 8, 11, 9] = [10, 8, 11, 9, 12, 10]

-- d) i. What is the most general type of filter?
-- filter :: (a -> Bool) -> [a] -> [a]
-- ii. Write down the function filter using recursion.


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
     | f x = x : (filter' f xs)
     | otherwise = filter' f xs

-- iii. Write down the function filter using list comprehension.

filterL :: (a -> Bool) -> [a] -> [a]
filterL l xs = [x | x <- xs, l x]

-- Write down (not using any library functions beyond the usual arithmetic and order operators) a function at that given a list of type [a] and a positive integer n returns the n-th
-- element of that list.

at :: Ord a => [a] -> Int -> a
at list n
    |n > (length list) = error "Outside range of list"
    |n == 0 = error "Outside range of list"
    |otherwise = at' list n 1

at' :: Ord a => [a] -> Int -> Int -> a
at' (x : xs) n c
   |n == c = x
   |otherwise = at' xs n (c+1)


countDif :: [Int] -> Int
countDif [] = 0
countDif list = countDif' list 0

check :: (Eq a) => [a] -> a -> Bool
check [] _ = False
check (x:xs) a
    | x == a = True
    | otherwise = check xs a


countDif' :: [Int] -> Int -> Int
countDif' [] n = n
countDif' (x:xs) n
    |check xs x = countDif' xs n
    |otherwise = countDif' xs (n+1)


--Question 2

minL :: [Int] -> Int
minL [] = 0
minL [x] =  x
minL (x:xs) = minL' x (minL xs)


minL' :: Int -> Int -> Int
minL' a b
   | a > b =  b
   | a < b = a
   | a == b = a


maxL :: [Int] -> Int
maxL []   =     0
maxL [x]  =     x
maxL (x:xs) = maxL' x (maxL xs)

maxL' :: Int -> Int -> Int
maxL' a b
   | a > b = a
   | a < b = b
   | a == b = a

measure :: [String] -> Int
measure xss = length(snd $ maximum $ [(length xs, xs) | xs <- xss])

extend :: Int -> String -> String
extend n str
    |n < length(str) = error "Number given is too small"
    |n >= length(str) = (extend' (n - length(str))) ++ str


extend':: Int -> String
extend' n = concat $ replicate n " "

extendrow :: [String] -> [String]
extendrow list = extendrow' list (measure list)

extendrow' :: [String] -> Int -> [String]
extendrow' [] n = []
extendrow' (x : xs) n = (extend n x) : extendrow' xs n

flatten :: Char -> [String] -> String
flatten c [] = []
flatten c (x : xs) = (c:x) ++ (flatten c xs)

extendmat :: [[String]] -> [[String]]
extendmat [] =  []
extendmat (x:xs) = (extendrow x) : extendmat xs

applyShow :: Show a => [[a]] -> [[String]]
applyShow mat = map (map show) mat

matFlatten :: [[String]] -> String
matFlatten mat = flatten '\n' (map(flatten ' ') mat)

-- prettyprint :: [[a]] -> IO()
-- prettyprint [] = return()
-- prettyprint (x:xs) = do putStr x
--                         putChar '\n'
--                         prettyprint xs


--EXAMP PAPER 2019

--1
--A
-- i. (2,0):[(1,9)] = (Int):[(Int)] = [(Int), (Int)]
-- ii. ’M’:"ay" = Char : String = String = [Char]
-- iii. ("2",([0],(1,9))) = (String,([Int],(Int,Int)))
-- iv. (<= 20*19) = Int -> Bool
-- v. [(x,x)|x<-"2019"] = [(Char, Char)]

--B
-- i. zip (a:as) (b:bs)= (a,b) : zip as bs
-- zip :: [a] -> [b] -> [(a,b)]

-- ii. zop (a:as) (b:bs)= (a,[b]) : zop as bs
-- zop :: [a] -> [b] -> [(a,[b])]

-- iii. zap (a:as) (b:bs)= (b,a) : zap as bs
-- [a] -> [b] -> [(b, a)]

--C
-- i. snd((2,0),1)+head[9]
-- 1 + 9 = 10

-- ii. drop 2 (take 3 (tail([2,0]++[1,9])))
-- drop 2 (task 3 (tail([2,0,1,9]))) = drop 2 (take 3 ([0,1,9])) = drop 2 ([0,1,9]) =  [9]

-- iii. filter not [False,True,False]
-- [False, False]

-- iv. map not [False,True,False]
-- [True, False, True]

--D
--map :: (a -> b) -> [a] -> [b]

--Map with Recursion

mapRe :: (a -> b) -> [a] -> [b]
mapRe _ [] =  []
mapRe f (x:xs) = f x : mapRe f xs

--Map with List Comprehension

mapL :: (a -> b) -> [a] -> [b]
mapL f xs = [f x | x <- xs]

data Candidate = Fulmar | Patel | Wren | Blanche | Petrisan | Twite
     deriving (Show, Enum)
type Name = String

transform :: [Candidate] -> [Name]
transform [] = []
transform (x:xs) = (transform' x) : transform xs
--transform list = map show list

transform' :: Candidate -> String
transform' c = show c

name2cand :: Name -> Candidate
name2cand n = head[c | c <- [Fulmar .. Twite], show c == n]

type Ballot = Name
data InspectedBallot = OK Name | Reject deriving (Show,Eq)

inspect :: Ballot -> InspectedBallot
inspect b 
  |elem b allCands = OK (name2cand b)
  |otherwise = Reject
  where
      allCands = transform [Fulmar .. Twite]
      
--inspect b = inspect' (name2cand b)

--inspect' :: Candidate -> InspectedBallot
--inspect' b
--       |b ==  = Reject
--       |otherwise = OK b
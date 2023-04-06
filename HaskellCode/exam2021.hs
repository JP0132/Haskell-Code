module Exam2021 where 

import Data.Ord
import Data.Char
import Data.List


--1
--a)
-- i. 20:[2+0]
-- [Int]

-- ii. (2.0,"2.0")
-- (Float, String)

-- iii. (id,sum,length)
-- (a2 -> a2, t1 a1 -> a1, t2 a3 -> Int)

-- iv. [("ab",x,"12")|x<-"2020"]
-- [(String,Char,String)]

-- v. putStr ("Sum"++[’m’,’e’,’r’])
-- String

--b)
-- i. bip (a:as) (b:bs) = (a,b) : bip as bs
-- bip :: [a] -> [b] -> [(a,b)]

-- ii. bap (a:as) (b:bs) = (b,a) : bap bs as
-- bap :: [a] -> [b] = [(b,a)]

-- iii. bop (a:as) (b:bs) = a : bop as bs 
-- bop :: [a] -> [b] -> [a]

--c)
-- i. drop 2 (’0’:"20") = 
-- drop 2("020") = "0"

-- ii. head (map fst [(0,8),(2,0),(2,0)]) =
-- head([0,2,2]) =  0

-- iii. map (3 *) [1..5] = 
-- map (3*)[1,2,3,4,5] = [3,6,9,12,15]

-- iv. head [False && True,True] = 
-- head [False,True] =  False

--d)

--map :: (a -> b) -> [a] -> [b]

--Map with Recursion

mapRe :: (a -> b) -> [a] -> [b]
mapRe _ [] =  []
mapRe f (x:xs) = f x : mapRe f xs

--Map with List Comprehension

mapL :: (a -> b) -> [a] -> [b]
mapL f xs = [f x | x <- xs]

--i
listcc :: [[a]] -> [a]
listcc [] = []
listcc (a:as) = a ++ listcc as

--ii
hmsort :: Ord b => (a -> b) -> [a] -> [a]
hmsort _ [x] = [x]
hmsort fun xs = hmerge fun (hmsort fun ys) (hmsort fun ws)
             where (ys,ws) = (take l xs, drop l xs)
                           where l = length xs `div` 2

hmerge :: Ord b => (a -> b) -> [a] -> [a] -> [a]
hmerge _ xs [] = xs
hmerge _ [] ys = ys
hmerge fun (x:xs) (y:ys)
       |fun x < fun y = x : hmerge fun xs (y:ys)
       |otherwise = y : hmerge fun (x:xs) ys



--2)
--a)
data BTree a = L a | B a (BTree a) (BTree a) deriving (Show,Eq)
data ATree a = K a | A a [ATree a]
instance Eq a => Eq (ATree a)
  where
      A _ [] == K a = True
      A _ [A _ []] == A _ [K _] = True
      A a [] == _ = False
      _ == K a = False



instance Show a => Show (ATree a)
   where
       show (A a (x:xs)) = "A " ++ (show a) ++ (show (x:xs))
       show (K b) = "K " ++ (show b) ++ " "
       show (A a []) = "K " ++ (show a)
             


--b)
-- countB :: BTree a -> Int
-- countB (B _ (L a) (L b)) = 1
-- countB (L a) = 1


-- countB :: BTree a -> Int
-- countB (L a) = 1
-- countB (B a (L _) (L _)) = 1 + 1 + 1
-- countB (B a (left) (L _)) = 1 + countB left + 1
-- countB (B a (L _) (right)) = 1 + 1 + countB right
-- countB (B a (left) (right)) = 1 + countB left + countB right



countA :: ATree a -> Int
countA (K a) = 0
countA (A a l) = countA' l + length l


countA' :: [ATree a] -> Int
countA' [] = 0
countA' [A _ l] = 1 + countA' l
countA' (x:xs) = 1 + countA' xs


countA2 :: [ATree a] -> [ATree a]
countA2 [] = []
countA2 [A _ l] = countA2 l
countA2 (x:xs) = x : countA2 xs

--countB (L a) = 0
--countB (B a left right) = 1 + countB left + countB right

--c)
-- btree2atree :: BTree a -> ATree a
-- btree2atree (B a left right) = 


--D)
flattenA :: ATree a -> [a]
flattenA (K a) =  [a]
flattenA (A a (x:xs)) = [a] ++ flattenA x ++ flattenA' xs

flattenA' :: [ATree a] -> [a]
flattenA' [K a] =  [a]
flattenA' [A a (x:xs)] =  [a] ++ flattenA x ++ (flattenA' xs)
flattenA' (x:xs) = flattenA' [x] ++ (flattenA' xs)

levelingA :: ATree a -> [a]
levelingA (K a) =  [a]
levelingA (A a (x:xs)) = [a] ++ levelingA x ++ levelingA' xs 

levelingA' :: [ATree a] -> [a]
levelingA' [K a] =  [a]
levelingA' [A a (x:xs)] =  [a] ++ levelingA' (xs)  ++ levelingA' [x]
levelingA' (x:xs) = levelingA' [x] ++ (levelingA' xs)


depthA :: ATree a -> Int
depthA (K a) = 0
depthA (A a (x:xs)) = 1 + depthA x + depthA' xs

depthA' :: [ATree a] -> Int
depthA' [K a] = 0
depthA' [A a (x:xs)] = 1+  depthA' [x] + depthA' (xs)
depthA' (x:xs) = depthA' [x] + depthA' xs



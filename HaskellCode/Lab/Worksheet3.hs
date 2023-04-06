--------------------------------------------------------------------
-- CO 2107  Functional Programming
-- Created: Feb 2021, University of Leicester, UK
--  12th February (not-assessed) 
--------------------------------------------------------------------
-- Student Name: Jaynik Parsotomo
-- Student Number: 199009725
--------------------------------------------------------------------
--


module Worksheet3 where
import Data.Char

----------------------------------------------------------------------
-- Exercise 1: 
---------------------------------------------------------------------

-- Write a function elementof that outputs True if a is an element of the list as 
-- and False otherwise
-- elementof 3 [1,2,3] = True
-- elementof 4 [1,2,3] = False

elementof :: Int -> [Int] -> Bool
elementof n [] = False
elementof n (x:xs) = n == x || elementof n xs 
  
  



----------------------------------------------------------------------
-- Exercise 2
---------------------------------------------------------------------

-- Write a function removeNth that given n and a list emoves every nth element
--removeNth 2 [1,2,3,4,5] = [1,3,5]
--removeNth 3 [1,2,3,4,5] = [1,2,4,5]

removeNth :: Int -> [Int] -> [Int]
removeNth n list = removeNthAux n 1 list

--removeNth n [] = []
--removeNth n (x:xs)
--  |n == 0 = xs
--  |otherwise = x : removeNth(n-1) xs

removeNthAux::Int -> Int -> [Int] -> [Int]
removeNthAux n i [] = []
removeNthAux n i (x:xs)
            | n == i  = removeNthAux n 1 xs
            | otherwise = x:removeNthAux n (i+1) xs




-----------
-- Look up how bubblesort works (wikipedia)

-- write down a function bubsort that orders a list of intergeres using bubblesort

bubsort :: [Int] -> [Int]
bubsort list = bubsort' list 1

--Does the swap
bubsort2 :: [Int] -> [Int]
bubsort2 [] = []
bubsort2 (x) = (x)
bubsort2 (x:y:xs)
-- Checks the head and the next element if x is larger a swap is made and x becomes the head of the list
  | x > y = y : bubsort2 (x:xs)
--If it isn't, x remains and y becomes the new head
  | otherwise = x : bubsort2 (y:xs)


--Does the swaps n times
bubsort' :: [Int] -> Int -> [Int]
bubsort' list n
--Checks if the list is the same size as n
  | n == (length list) = list
--If not then carry on swapping until it does, increase n by 1 to keep track
  | otherwise = (bubsort' (bubsort2 list) (n + 1))

--From lecture bubble sort
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (a:b:cs)
  | a < b = a : bubble (b:cs)
  | otherwise = b : bubble(a:cs)

bubbleAux :: Ord a => Int -> [a] -> [a]
bubbleAux 0 as = as
bubbleAux n as = as
bubbleAux n as = bubbleAux (n-1) (bubble as)

bsort2 :: Ord a => [a] -> [a]
bsort2 as = bubbleAux (length as) as


bsort :: Ord a => [a] -> [a]
bsort xs
  | xs = bubble xs = xs
  | otherwise = bsort (bubble xs)
     where zs = bubble xs 


-----------------------------------------------------------
-- Exercise 4
---------------------------------------------------------------------

-- write a function int2str that given an integer n produces the string ofd digits of n
-- don't use show!

--int2str [125] = "125"

--int2str :: Int -> String
--int2str n = [mod n 10]++ [div n 10]

 

-----------------------------------------------------------------
-- Exercise 5
-----------------------------------------------------------------

-- and write the reverse function that given a string of digit prduces the corresponding integer

-- str2int "512" = 512

-- str2int :: String -> Int


-- Now write a function reverseint that takes an integer and reveres its digits

-- reverseint 123 = 321

--reverseint :: Int -> Int


-----------------------------------------------------------------
-- Exercise 6: 
-----------------------------------------------------------------

-- remember matrices. We think of them as list of list of integers

--   | 1,2,3,4 |
--   | 6,7,8,9 |    in Haskell [[1,2,3,4],[6,7,8,9]] (a row of rows)

-- Write a function that transforms an  n by m  matrix in a  m by n matrix   
-- element b(i,j) of the new matrix is element a(j,i) from the old matrix

-- example:

--            | 1,2,3,4 |
-- transform  | 6,7,8,9 | = | 1,6 |
--                          | 2 7 |
--                          | 3,7 |
--                          | 4,8 |


-- transform [[Int]] = [[Int]]  



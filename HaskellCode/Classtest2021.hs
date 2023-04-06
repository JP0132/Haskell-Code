-------------------------------------------------------

-- CO2107 Functional Programming
-- Classtest 26/02/2021
-- Handin also via Blackboard

{------------------------------------------------------

This test is worth 50 marks in total.

You have a 24 hour slot for handing in beginning at 
Friday 12.00 am (noon) and ending Saturday 12.00 (noon)

In question 4 and 5 you are supposed to write working code. 
Take care that the whole classtest.hs properly compiles with ghci: 
if it does not, please, comment the faulty lines of code out 
and indicate where the problem is. 

As always the answers should be your own answers. 
If you use want to use functions from the prelude, that is OK, 
but then briefly explain what those functions are doing.

READ CAREFULLY!

--}

module Classtest where 
import Data.Char hiding (filter)


{-------- Question 1 ----- [10 marks]

What are the most general types of the following expressions
(you may assume that the type of the used numbers is Int)

--a  [[1,2],[3,4]] :: [[Int,Int],[Int,Int]]  Correct Answer [[Int]]

--b  (",",’,’,[’]’]) :: (String,Char,[Char])

--c  show [2021] :: String

--d  map :: (a -> b) -> [a] -> [b]

--e  [head ['a']] :: [Char]

--}



{---------- Question 2 -------- [8 marks]
Give the (most general) type declarations for the following function definitions:
-- a  pair :: a -> (a,a)
      pair x = (x,x)
       
-- b  fun :: (a -> a -> a) -> Int -> [a] -> (a -> a -> a)   Correct Answer: (a -> b -> b)
      fun f z (x:xs) = f x (fun f z xs)
      
--}


{---------- Question 3 ------ [8 marks]
Evaluate step by step:

--example:  head (tail "string") = head "tring" = 't'

--a   (tail [2,0,2,1]):[] = [0,2,1] : [] = [[0,2,1]]

--b   1 + fst (head [(2,0),(2,1)]) = 1 + fst ((2,0)) = 1 + 2 =  3

--c   head [tail [2..4] ++ tail [3..6]] = head [[3,4]] ++ [4,5,6]] = head [[3,4,4,5,6]] = [3,4,4,5,6]

--d   map (3/=) (tail [fst(2,0), 2+1, snd(26,2)]) = map (3/=) (tail [2, 3, 2])) = map(3/=) [3,2] = [False, True]

--}



{---------- Question 4 ----- [8 marks]

--a   What is the most general type of filter?

--} --filter :: (a -> Bool) -> [a] -> [a]

--b   Write down the function filter using recursion. (use the name filter1)

-- filter1 :: (a -> Bool) -> [a] -> [a]
-- filter1 _ [] = []
-- filter1 f (x:xs)
--        | f x        = x : filter1(f xs)
--        | otherwise  = filter1(f xs)

--c   Write down the function filter using list comprehension. (use the name filter2)

--filter2 :: (a -> Bool) -> [a] -> [a]
--filter2 f xs = [x | x <- xs, f x]



{------------Question 5 ------ [16 marks in total]

We consider non-empty lists of non-empy lists of integers.  
We say that a list [l1,...,ln] ::[[Int]]
of n non-empty lists of integers is a TRIANGLE
if the length of each li = i for 1 <= i <= n. --}

--Examples: 

t1,t2,t3 :: [[Int]]
t1 = [[1]]
t2 = [[1],[2,3]]
t3 = [[1], [2,3],[4,5,6]]

--Non-examples:

n1,n2,n3,n4 :: [[Int]]
n1 = []
n2 = [[1],[4,5,6]]
n3 = [[],[2,3],[4,5,6]]
n4 = [[4,5,6],[3,2],[1]]


{-- 5a [6 marks]
Write down a function isTriangle :: [[Int]] -> Bool
such that outputs True if its input is a triangle (according to the above definition) 
and False otherwise.
--}

-- check :: (Eq a) => [[a]] -> a -> Bool
-- check [] _ = True
-- check (x:xs) i
--       |length (head(x)) == (i+1) = check xs (i+1)
--       |otherwise = False

-- isTriangle :: [[Int]] -> Bool
-- isTriangle (x:xs)
--       |length(head(x:xs)) /= 1 = False
--       |otherwise = check xs length(head(list))

check :: (Eq a) => [[a]] -> Int -> Bool
check [] _ = True
check (x:xs) i
      |length(x) == i = check xs (i+1)
      |otherwise = False

isTriangle :: [[Int]] -> Bool
isTriangle [] = False
isTriangle (x:xs)
      |length(x) /= 1 = False
      |otherwise = check xs 2


-- isTriangle' :: [[Int]] -> Int -> Bool
-- isTriangle' (x:xs) i
--       |check xs i 



{-- 5b [10 marks]
Write down a function rotateA ::Triangle -> Triangle
that "rotates" a triangle anti-clockwise as suggested by the pictures

     1
    2 3   
   4 5 6  represented by [[1],[2,3],[4,5,6]]
   
rotates anticlockwise to the triangle
     6
    3 5
   1 2 4  represented by [[6],[3,5],[1,2,4]] 

Examples:

rotateA [[1]] = [[1]]

rotateA [[1],[2,3]] = [[3],[1,2]]
            1             3  
           2 3           1 2
   
rotateA [[1],[2,3],[4,5,6]] = [[6],[3,5],[1,2,4]]
            1                     6
           2 3                   3 5 
          4 5 6                 1 2 4 

Etc. 

Feel free to define any functions that may help you... 
Please briefly explain your answer.
--}



type Triangle = [[Int]]

rotateA :: Triangle -> Triangle
rotateA list = reverse(rotateA' list)


rotateA' :: Triangle -> Triangle
rotateA' []              = []
rotateA' ([]:xss)        = rotateA' xss
rotateA' ((x:xs) : xss)  = (x : [x |  (x:_) <- xss]) : rotateA' (xs : [xs | (_:xs) <- xss])


{--
My first idea was to convert the triangle into a 1d array thenbrotate the list then convert it back a triangle 
but couldn't find a way to rotate the 1d to match the 2d array
So instead i used list comprehension in  rotateA'. It basically gets the head list (x) and tail list(s) (xs), and then xss gets the 2d array
from the xss it gets the head of the list and then adds that to the list this is then appended to the head list (x). The same thing is done to the tail of
the list but this part of the list is then passed through rotateA' so items within all the list are rotated. The recursion breaks when the list that is passed is empty.
Also in rotate A i have used the built in function reverse as rotateA' output moves each of the list within the 2d in the opposite order.

--}

-- oneList :: [[Int]] -> [Int]
-- oneList [] = []
-- oneList (x:xs) = x ++ (oneList xs)

--rotate2 :: [Int] -> [Int]
--rotate2 [] = []
--rotate2 (x:xs) = xs ++ [x]

--convert2d :: [Int] -> [[Int]]

-- head' :: [a] -> [a]
-- head' [] = []
-- head' (x:_) = [x]

-- tail' :: [a] -> [a]
-- tail' [] = []
-- tail' (_:xs) = xs

-- rotateA :: [[a]] -> [[a]]
-- rotateA list = reverse(rotateA' list)


-- rotateA' :: [[a]] -> [[a]]
-- rotateA' []              = []
-- rotateA' ([]:xss)        = rotateA' xss
-- rotateA' ((x:xs) : xss)  = (x : [x |  (x:_) <- xss]) : rotateA' (xs : [xs | (_:xs) <- xss])


module Lec7 where
import Data.Char
import Prelude hiding (append)



--RECURSION

myappend :: [a] -> [a] -> [a]
myappend [] list         = list
myappend (x:xs) list     = x : (myappend xs list)

--head :: [a] -> a
--head [] = errorLec
--head (x:xs) = x
--Outputs the first element in the list

--tail :: [a] -> [a]
--tail [] = error
--tail (x:xs) = xs
--Outputs the last element in the list

take2 :: Int -> [a] -> [a]
take2 n [] = []
--take n (x:xs) = take (n-1) xs
take2 0 list = []
take2 n (x:xs) = x : take2(n-1) xs
--Takes the elements in the list from 1 to n

drop2 :: Int -> [a] -> [a]
drop2 0 list = list
drop2 n [] = []
drop2 n (x:xs) = drop (n-1) xs
--Takes the elements after 1 to n

zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] list = []
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys
--Pairs the 2 list together

double2 :: [Int] -> [Int]
double2 [] = []
double2 (x:xs) = (2*x) : double2 xs
--Doubles the element in the list - multiples them by 2

append :: [a] -> [a] -> [a]
append [] x = x
append (x:xs) y = x : append xs y

mymap :: (a -> b) [a] -> [b]
mymap f [] = []
mymap f (x:xs) = (f x) : (mymap f xs)
--Map function using recursion


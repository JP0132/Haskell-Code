module Lec6 where
import Data.Char
import Numeric

--Remove Duplicates

rdup :: Eq a => [a] -> [a]
rdup [] = [] --Checks if the list is empty
rdup (a:as) = a : (rdup[ x | x<-as , x /= a]) 
-- Using list comprehession checks the item in the list and compares it to to check if they are equal
--using recursion to call rdup to remove duplicates again

--Set up our own data type
data Nat = Z | S Nat deriving Show


one,two :: Nat
one = S(Z) --S stands for successor

two = S(one)

add :: Nat -> Nat -> Nat
add n Z = n
add n (S m) =   S(add n m) 

addIndex :: [Int] -> [(Int,Int)]
addIndex xs = pairs 1 xs


pairs :: Int -> [Int] -> [(Int,Int)]
pairs n [x] = [(n,x)]
pairs n (x:xs) = (n,x):pairs (n+1) xs
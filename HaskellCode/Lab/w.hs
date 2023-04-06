--------------------------------------------------------------------
-- CO2107  Functional Programming  
-- Created: October 2020, University of Leicester, UK                        
-------------------------------------------------------------------- 
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--                       
--------------------------------------------------------------------           
-- Student Name: Jaynik
-- Student Number: 19909725
--------------------------------------------------------------------

module Worksheet0 where 
import Data.Char

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]
--------------------------------------------------------------------
-- Exercise 7

--------------------------------------------------------------------

myint :: Int
myint = 707


myfloat :: Float
myfloat = 12.5


mychar :: Char 
mychar = 't'


mystring :: String
mystring = "Hello "

less :: Bool
less = (myint < 100)


cube :: Int -> Int
cube n = n*n*n

--------------------------------------------------------------------
-- Exercise 8
--------------------------------------------------------------------


-- A function with two integer input that adds them.
--plus :: Int -> Int -> Int 
--plus m n = ?


-- A function with three integer inputs and Boolean output;
-- yields True if all inputs equal, else False.
--allEqual :: Int -> Int -> Int -> Bool
--allEqual m n k =  ?

--------------------------------------------------------------------
-- Exercise 11
--------------------------------------------------------------------

-- A function that gives the message "The number is XXX"

--message :: Float  -> String
--message x = ?



--------------------------------------------------------------------
-- Exercise 12
--------------------------------------------------------------------

-- Do 13 first

--Note we did not give you the type declaration...


--------------------------------------------------------------------
-- Exercise 13
--------------------------------------------------------------------

{-
N = a 'div' length xs
    where
      a = 10
     as = [1,2,3,4,5]
-}

{- Note the above function definition is surrounded by comment indicators.
Please remove them,
and reload the worksheet
(use command :r in GHCi) -}

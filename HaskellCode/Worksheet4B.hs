--------------------------------------------------------------------
-- CO2107  Functional Programming                            
-- Created Feb 2021, University of Leicester, UK                        
--------------------------------------------------------------------           
-- Student Name
-- Student Number
--------------------------------------------------------------------

module Worksheet4B where 
import Data.Char


----------------------------------------------------------------------
-- Exercise 4
---------------------------------------------------------------------

homerge :: Ord b => (a -> b) -> [a]  -> [a] -> [a]
homerge f [] bs = bs
homerge f as [] = as
homerge f (a:as) (b:bs)
  | f(a)<=f(b)  = a:(homerge f as (b:bs))  
  | otherwise   = b:(homerge f (a:as) bs)  


hoMergeSort :: Ord b => (a -> b) -> [a] -> [a]
hoMergeSort f [] = []
hoMergeSort f [x] = [x]
hoMergeSort f xs = homerge f (hoMergeSort f ys) (hoMergeSort f ws)
             where (ys,ws) = (take l xs, drop l xs)
                   l = length xs `div` 2

test :: [String]
test = hoMergeSort length list

list :: [String]
list = ["er","staat","een","paard","in","de","wei"]
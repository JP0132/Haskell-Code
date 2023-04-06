module Lec5 where
import Data.Char

names :: [String] -> [String]
names l = [ e | e <- l, head e == 'A']

doubleEven :: [Int] -> [Int]
doubleEven l =[ 2*e | e <- l, isEven e]




quickSort' :: (Ord a) => (a -> a -> Ordering) -> [(a,b,c)] -> [(a,b,c)]
-- No matter how we compare two things the base case doesn't change,
-- so we use the _ "wildcard" to ignore the comparison function.
quickSort' _ [] = []

-- c is our comparison function
quickSort' c (x : xs) = (quickSort' c less) ++ (x : equal) ++ (quickSort' c more)
    where
        less  = filter (\y -> fst3(y) `c` fst3(x) == LT) xs
        equal = filter (\y -> fst3(y) `c` fst3(x) == EQ) xs
        more  = filter (\y -> fst3(y) `c` fst3(x) == GT) xs

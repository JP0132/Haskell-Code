module Lec8 where
import Data.Char

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n
  | n < 0 = error "Negative Input not allowed"
  | otherwise = fib (n-1) + fib (n-2)

--Insertion Sort

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs)
  |n <= x = n:(x:xs) --Checks if n is smaller or equal to the x (the head of the list) if yes insert x before x
  |n > x = x:(insert n xs) --Checks n is larger then x, if so x is remained in the same place and usign recursion insert is called with n and rest of the list (xs)

--Used to insert the number in the right place in the list

insort :: [Int] -> [Int]
insort [] = []
insort (x:xs) = insert x (insort xs) -- calls insert on x (the first element) with insort with the rest of the list
--Sort the list using insertion

inssort :: Ord a => [a] -> [a]
inssort [] = []
inssort (x:xs) = insert x (inssort xs)
  where insert n [] = [n]
        insert n(y:ys)
          | n <= y = n : y: ys
          | n > y = y : (insert n ys)

-- In functional programming you can capture the essence of the algorithm, with all the implementation detail!

--Quick Sort

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort smaller) ++ [x] ++ (qsort larger) -- adds the list of smaller elemetns and list of larger elements to x (the pivot)
--Recursion is used to sort the 2 list
  where smaller = [y | y <- xs, y <= x] -- List comprehension to check if the element is smaller then x
        larger  = [y | y <- xs, y > x ] -- List comprehension to check if the element is larger then x

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((a,b): ps) = (a:as, b:bs)
  where as = fst(unzip' ps)
        bs = snd (unzip' ps)



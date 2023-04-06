module Worksheet7 where
--i
minL :: [Int] -> Int
--minL [] = []
minL [] = 0
minL [x] = x
minL (x:xs) = minL' x (minL xs)


minL' :: Int -> Int -> Int
minL' a b
     | a > b = b
     | a < b = a
     | a == b = a

--ii
maxL :: [Int] -> Int
--minL [] = []
maxL [] = 0
maxL [x] = x
maxL (x:xs) = maxL' x (maxL xs)


maxL' :: Int -> Int -> Int
maxL' a b
     | a > b = a
     | a < b = b
     | a == b = a

--iii
-- measure :: [String] -> String
-- measure [] = []
-- measure (x:xs) = measure' x (measure xs)

-- measure' :: String -> String -> String
-- measure' a b
--   | length(a) > length (b) = a
--   | length(a) < length (b) = b
--   | length(a) == length (b) = b

measure :: [String] -> [String]
measure list = [a | a <- list, length(a) < length(a)]

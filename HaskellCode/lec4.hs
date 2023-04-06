module Lec4 where
import Data.Char

sq :: Int -> Int
sq x = x^2

cube :: Int -> Int
cube x = x^3

evens2 :: Int -> [Int]
evens2 n = filter isEven [1 .. n]
    where isEven x = (x `mod` 2 == 0)


evens3 :: Int -> [Int]
evens3 n = map (*2) [1..(n`div`2)]

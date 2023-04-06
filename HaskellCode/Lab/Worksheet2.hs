--------------------------------------------------------------------
-- CO2107  Functional Programming
-- Created: Feb 2021, University of Leicester, UK
-- handin 9.00 hr on paper on 6th February (assessed) 
--------------------------------------------------------------------
-- Student Name: Jaynik Parsotomo
-- Student Number:199009725
--------------------------------------------------------------------
--
-- Please don't hand in buggy solutions. That makes the marking harder.
-- Points may be deducted if your solution does not compile properly...
-- use a good looking layout

module Worksheet2 where
import Data.Char


-----------------------------------------------------------------
-- Exercise 1:  Customers of a Bank
-----------------------------------------------------------------

type NI = Int
type Age = Int
type Balance = Float
type Customer  = (NI,Age, Balance)
type Bank =  [Customer]

-- Part a)
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

retired :: Customer -> Bool
retired c
        |age >= 67 = True
        |otherwise = False
         where age = snd3 (c)

-- Part b)

deposit :: Customer -> Float -> Customer
deposit (ni, a, b) dep  = (ni, a , b+dep)
  
-- Part c)
withdraw :: Customer -> Float -> Customer
withdraw (ni, a, b) with
  |with  > b    = error "Balance not enough"
  |with  <= b   = (ni, a, b-with)

-- Part d)
credit :: Bank -> [Customer]
credit bank = [ (ni,a,bl) | (ni,a,bl) <- bank, bl>0]

--credit bank = filter(\(ni,a,bl) -> c>0) bank




-----------------------------------------------------------------
-- Exercise 2: addindex
-----------------------------------------------------------------

addIndex :: [Int] -> [(Int,Int)]
addIndex ilist = zip [1..] ilist




------------------------------------------------
---- Exercise 3: reproduce  
---------------------------------

reproduce :: Int -> String -> [String]
reproduce n w =  [1..n] >> [w]

--reproduce n w =  [1..n] * [w]

--reproduce n string should repeat the input string n times
-- example
--reproduce 3 "test" = ["test","test","test"]




------------------------------------------------
---- Exercise 4: encode 
---------------------------------

code :: Int -> Char -> Char
code n ch 
        | isUpper ch   = chr $ ((ord ch - ord 'A' + n) `mod` 26) + ord 'A'
        | isLower ch   = chr $ ((ord ch - ord 'a' + n) `mod` 26) + ord 'a'
        | otherwise    = ch

encode :: Int -> String -> String
encode n str = map (code n) str

--encode n str = map(code n str) str 




---------------------------------------------------------------------
-- Exercise 5  escape characters
---------------------------------------------------------------------

rawtext :: String
rawtext = "\"This is a \\ \\long string,\n\\ \\ spanning multiple lines,\nin fact 3 lines!\""
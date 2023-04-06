--------------------------------------------------------------------
-- CO 2008  Functional Programming
-- Created: October 2018, University of Leicester, UK
-- Revised: jan 2020
--------------------------------------------------------------------
--
-- DON'T FORGET TO FILL IN NAME AND STUDENT NUMBER.
--
-------------------------------------------------------------------
-- Student Name
-- Student Number
--------------------------------------------------------------------

module Worksheet1 where
import Data.Char
import Numeric

-----------------------------------------------------------------
-- Exercise 1
-----------------------------------------------------------------

type Verb  = String

pastTense :: Verb -> Verb
pastTense  v = v++"ed"

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------
type Mass = Float
type Height = Float
type BMI = Float

bmi :: Mass -> Height -> BMI
bmi m h = m / (h^2) 

-- using formula from https://en.wikipedia.org/wiki/Body_mass_index:
-- take mass divided by square of your height

----------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------

type NumberOfCars = Int
type DailyCost  = Float

cost :: NumberOfCars  -> DailyCost
cost n
  | n < 0                 =  error "Car production is always positive"
  | 0 <= n   && n <= 500  =  5*m + 1000
  | otherwise             =  10*m + 450
   where m = fromIntegral n

---------------------------------------------------------------------
-- Exercise 4. 
---------------------------------------------------------------------

type Year = Int

currentyear = 2019 :: Int

age :: Year -> Int
age year
        | year >= 100 || 0 > year      = error "input negative or larger than 100"
        | year <= (currentyear - 2000) = (currentyear - 2000) - year
        | year < 100 && 0 <= year      = (currentyear - (1900 + year))    

---------------------------------------------------------------------
-- Exercise 5. Pounds Euros
---------------------------------------------------------------------

type Euros = Float
type Pounds = Float
eurocurrency = 1.14 :: Float

p2e  :: Pounds -> Euros
p2e pound = eurocurrency * pound

e2p :: Euros -> Pounds
e2p euro = euro/eurocurrency

---------------------------------------------------------------------
-- Exercise 6. Pretty Print
---------------------------------------------------------------------
{- copying from the web does not learn us much

prettyprintEuro  x = 'â‚¬':showGFloat (Just 2) x
prettyprintPound x = 'Â£':showGFloat (Just 2) x

we can do it ourselves
-}



twoDecimals :: Float -> Float
twoDecimals x = (fromIntegral (round (x*100))) /100

prettyprintEuro :: Euros -> String
prettyprintEuro  x = 'â‚¬':(show (twoDecimals x))

prettyprintPound :: Pounds -> String
prettyprintPound x = 'Â£':(show (twoDecimals x))


---------------------------------------------------------------------
-- Exercise 7.  escape characters
---------------------------------------------------------------------

rawtext :: String
rawtext = "\"this is a \\ \\long string,\n\\ \\ spanning multiple lines,\nin fact 3 lines!\""


---------------------------------------------------------------------
-- Exercise 8. removeZeroes
---------------------------------------------------------------------

removeZeroes :: [Int] -> [Int]
removeZeroes list = filter (/= 0) list

---------------------------------------------------------------------
-- Exercise 9.  capslockon
---------------------------------------------------------------------

lowercase :: Char -> Bool
lowercase char = ord char >= ord 'a' &&  ord char <= ord 'z' 

uppercase :: Char -> Bool
uppercase char = ord char >= ord 'A' &&  ord char <= ord 'Z' 

capslockon :: String -> String
capslockon text = map f text
    where f char
            | lowercase char = chr ((ord char) -  ord 'a' + ord 'A')
            | uppercase char = chr ((ord char) -  ord 'A' + ord 'a')
            | otherwise      = char


---------------------------------------------------------------------
-- Exercise 10
---------------------------------------------------------------------

listOffASCICharacters :: String
listOffASCICharacters = map chr [0..127]

listOffAllCharacters :: String
listOffAllCharacters = map chr [0..1114111]

{- one discovers this by trying
bigger and bigger numbers, say add a 0 ach time:
then map chr [0..100000000]
gives error message
\1114111*** Exception: Prelude.chr: bad argument: 1114112
this reveals the last number that still points at a character...

So there are 1114112 characters, the first 128 of these are our ASCI characters.
For the rest see the explanation of UNICODE on websites like
https://flaviocopes.com/unicode/
https://unicodebook.readthedocs.io/unicode.html
-}

---------------------------------------------------------------------
-- Exercise 11.  removeZeroes2
---------------------------------------------------------------------

--example
--removeZeroes2 1020304 = 1234
--removeZeroes2 0 = 1234

removeZeroes2 :: Int -> Int
removeZeroes2 0 = error "input should not be 0"
removeZeroes2 int = read(filter ('0'/=) (show int))

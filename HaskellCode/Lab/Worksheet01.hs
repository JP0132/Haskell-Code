--------------------------------------------------------------------
-- CO 2008  Functional Programming
-- Created: January 2021, University of Leicester, UK
--------------------------------------------------------------------
-- Student Name
-- Student Number
--------------------------------------------------------------------
--
-- Please don't hand in buggy solutions. That makes the marking harder.
-- Points may be deducted if your solution does not compile properly...
-- use a good looking layout

module Worksheet1 where
import Data.Char
import Numeric

-----------------------------------------------------------------
-- Exercise  (done in class)
-----------------------------------------------------------------

type Verb  = String

--pastTense :: Verb -> Verb


----------------------------------------------------------------------
-- Exercise (done in class)
----------------------------------------------------------------------
type Mass = Float
type Height = Float
type BMI = Float

--bmi :: Mass -> Height -> BMI


-- using formula from https://en.wikipedia.org/wiki/Body_mass_index:
-- take mass divided by square of your height

----------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------

type NumberOfCars = Int
type DailyCost  = Float

cost :: NumberOfCars  -> DailyCost
cost n
    |n < 0                = error "wrong"
    |n < 0 && n <= 500    = (5*fromIntegral(n))+100
    |otherwise            = (10*fromIntegral(n)) + 450

--Align it nicely to give it a clear layout to read

---------------------------------------------------------------------
-- Exercise 2. Pounds Euros
---------------------------------------------------------------------

type Euros = Float
type Pounds = Float
eurocurrency = 1.14 :: Float

p2e  :: Pounds -> Euros
p2e p = p * eurocurrency

e2p :: Euros -> Pounds
e2p e = e / eurocurrency

pretty_p2e :: Pounds -> String
pretty_p2e p = '€' : show (p2e p)

----------------------------------------------------------------------
-- Exercise 3: A phone book
---------------------------------------------------------------------

type Name = String
type PhoneNumber = Int
type Person  = (Name, PhoneNumber)
type PhoneBook = [Person]

-- Part a)

add :: Person -> PhoneBook -> PhoneBook
add p pb = p : pb

-- Part b)

delete  :: Name -> PhoneBook -> PhoneBook
delete n pb = filter (test n) pb 
    --where test n person = n /= fst person
    where test n (name,p) = name /= n
--  Part c)

find  :: Name -> PhoneBook -> [PhoneNumber]
--find name pBook = [p | (n,p) <- pBook, (n == name)]
    --where test n (name,p) = name == n
--The solution above uses list comprehension

find n pb = map snd (filter (test n) pb)
    where test n (name,phone) =  name == n

--Using Filter to find the phone number, in to its own list that contains the [(name,phonenumber)] 
--then usign map snd to get just the phonenumber from the filtered list [phonenumber]

--  Part d)

update :: Name ->  PhoneNumber -> PhoneNumber-> PhoneBook -> PhoneBook
update name oldtel newtel oldbook = [ f (n,tel) | (n,tel) <- oldbook ]
  where f (n, tel)
         | n==name = if oldtel == tel then (n,newtel) else  (n,tel)
         | n/=name = (n,newtel)


---------------------------------------------------------------------
-- Exercise 4  escape characters
---------------------------------------------------------------------

rawtext :: String
rawtext = "\"This is a \\ \\long string,\n\\ \\ spanning multiple lines,\nin fact 3 lines!\""




rdup :: Eq a => [a] -> [a]
rdup [] = [] --Checks if the list is empty
rdup (a:as) = a : (rdup[ x | x<-as , x /= a]) 
-- Using list comprehession checks the item in the list and compares it to to check if they are equal
--using recursion to call rdup to remove duplicates again


--------------------------------------------------------------------
-- CO2107  Functional Programming
-- Created: March 2021, University of Leicester, UK
-- (not-assessed) 
--------------------------------------------------------------------



module Worksheet5 where
import Data.Char

--Whereever you see   = error"???"  below you are suppposed to replace
--it with meaningful code that properly compiles.

---------------------------------------------------------------------
----- EXERCISE 1
---------------------------------------------------------------------


{--
A pack of playing cards contains 52 cards. Each card has a â€™valueâ€™ which
is taken to be an element of the type.
--}

data Value = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K|A
             deriving (Eq, Ord, Enum)


--and it has a â€˜suiteâ€™ which is taken to be an element of the type

data Suite = Hearts | Spades | Diamonds| Clubs
             deriving (Eq, Ord, Enum)

--A card is thus an element of

type Card = (Value, Suite)



{--
So we have introduced 3 datatypes: Value, Suite and Card

Recall what deriving (Eq,Ord,Enum) means.
Why don't we have to do that for Card???

In a moment you will to define your own show functions for
the new datatype Value and Suite.
--}

--- Part a) -----------------
  
instance Show Value
  where show = error"???"


--- Part b) ----------------

instance Show Suite
  where show = error"???"

--- Part c) ---------------

{--
Is it possible to write a show function for Card that
would transform (A,Heart) into the string AH.
--}

--- Part d)

-- Give a concise definition of a concrete value pack::[Card]
-- containing all of the possible playing cards in some order.
-- Hint: Use list comprehension.

pack :: [Card]
pack = error"???"

--- Part e)

--There are two colours of playing cards

data Colour = Red | Black
              deriving (Eq, Ord,Enum, Show)

--A card is Red if its suite is either Diamonds or Hearts and is Black
--otherwise. Write a function to determine the colour of a card.

colour :: Card -> Colour
colour = error"???"

{-- For the next part we will use a new data type Error a, that we
will use as output type for functions that would otherwise stop the
execution with an error message. Now the output Fail can be used by a
next function to do do something meaning ful to recover the error
situation. If the functions produce correwct output, we wrap this with
Ok.  --}

data Error a = Fail | Ok a
               deriving (Eq, Ord, Show)


--- Part f) ------------

{--
A common way to shuffle a pack of cards is to repeatedly split the
pack roughly in the middle and then to interleave the two por-
tions. Write a function
--}


split::Int->[a]-> Error ([a],[a])
split n list = error"???"

{-- so that

split n

divides a list in two at the point just after the n-th
element (we start counting from n=1).

For instance,

split 0 [1,2, 3,4,5] = Ok ([], [1,2,3,4,5])
split 2 [1,2, 3,4,5] = Ok ([1,2], [3,4,5])

Give an error in case the number is negative or larger than the length
of the list. For instance,

split 8 [1,2, 3,4,5] = Fail
split (-5) [1,2, 3,4,5] = Fail

Write a second function to interleave two lists of type [a], possibly of
different lengths.

For instance, interleaving two list of integers looks like this:

interleave [1,2,3] [4,5,6,7,8,9] = [1,4,2,5,3,6,7,8,9].
--}

interleave :: [a] -> [a] -> [a]
interleave  = error"???"


--- Part g)

{--
A shuffle of the pack is specified by giving a list of integers. For
example, the list standard below corresponds to the shuffle in which
the pack is split after the 23rd card, interleaved, split again after the
26th card, interleaved, and so on.
--}

standard :: [Int]
standard = [23,26,25,31,19,27]

{--
Write a function shuffle which can be used on any list xs
(standard is just an example) to calculate the effect of shuffling
the list according to a list of integers.

Use the functions split and interleave defined in the previous
part.

Give an error in case split gives an error, i.e. give an error in
case there is a number which is negative or larger than the length of
the list xs.
--}

shuffle :: [Int] -> [a] -> Error [a]
shuffle = error"???"


---------------------------------------------------------------------
----- EXERCISE 2
---------------------------------------------------------------------

{--
A binary tree can be used as a database. Here, the leaves of a tree are
either ND indicating no data, or Data d where d is a data item.
--}

data Btree a = ND | Data a |  Branch (Btree a) (Btree a)
               deriving (Show,Eq)

--some sample trees

tree1 = Branch ND ND
tree2 = Branch ND (Data 3)
tree3 = Branch tree1 tree2
tree4 = Branch (Data 3) (Data 4)
tree5 = Branch tree3 tree4

{--
One can give a path to a leaf by giving a list such as
[L,R,L]
which indicates the leaf one arrives at by moving
left, right, left
from the root of the tree (of course, there may be no such leaf).
--}

data Dir = L | R 
           deriving (Show,Eq)

type Path =  [Dir] 
    
--- Part a) ------------------

{--
Define extract which given a path and a binary tree, outputs the
data at the end of the path, and gives an error value when the path
does not match any data.
--}

extract :: Path  -> Btree a -> Error a
extract = error"???"

--- Part b) -----------------

{--
Define add, whose three inputs are some data, a path, and a binary
tree. The output consists of the binary tree, modified to include the
data item at the end of the path. In more detail, if the input path
ends in a leaf node of the form ND the tree is extended to contain the
new data. If the path leads to a branching node or a leaf node of the
form Data d an error value is given.
--}

add :: a -> Path -> Btree a -> Error (Btree a)
add = error"???"

--- Part c)

{--
Suppose the tree holds data of type a. Define the function findpath,
which given a function f, some data x and a tree t, returns the lists
of paths (possible empty) in t to the nodes of the form Node d where
f d is equal to x.
--}

findpath :: Eq b => (a -> b) -> b -> Btree a -> [Path]
findpath = error"???"

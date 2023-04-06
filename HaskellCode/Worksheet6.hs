
-- CO2107 Functional Programming 
-- Created: March 2021, University of Leicester, UK 
-------------------------------------------------------------------- 
{--
Handin by 5 pm on Wednessday 24 March 2021.

Question 1 is worth 30 points, Question 2 is worth 100 points.
Final mark will be a percentage out of 100.

Replace the "= error"???" phrases below by working code.

Take care that you  in your own work.

Good Luck!
--}


module Worksheet6 where 

import Data.Ord
import Data.Char
import Data.List



---------------------------------------------------------------------
----- EXERCISE 1
---------------------------------------------------------------------

{--Suppose we have a spreadsheet containing triples (name, user,mark) where

 - name is the name of a student,
 - user is his username and
 - mark is the result of the student’s first CW.

We wish to be able to order such spreasheets either by name, username or mark.

For example; if we sort this sample spreadsheet

  sortMark[("Socrates","ps21",60),("Xantippe","x12",80 ),("Cleo","cl123",70)]

by MARK in descending order (higher marks first),
we should get the spreadsheet

  sortMark[("Xantippe","x12",80 ),("Cleo","cl123",70),("Socrates","ps21",60)]


We introduce the following datatypes:
--}

type Name      = String
type Username  = String
type Mark      = Int

type Spreadsheet =  [(Name,Username,Mark)]


{--------- 1a  (10 marks) -----------------------------------

Write a function--}


sortName :: Spreadsheet -> Spreadsheet
sortName sp = qSortName' comp' sp

-- compare' :: Int -> Int -> Ordering
-- compare' a b
--   | a > b = GT
--   | a < b = LT


comp' :: (Ord a) => a -> a -> Ordering
comp' a b
   | a >  b = GT
   | a <  b = LT
   | a == b = EQ



fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- qSortName :: Ord a => [(a,b,c)] -> [(a,b,c)]
-- qSortName [] = []
-- qSortName (x:xs) = (qSortName smaller) ++ [x] ++ (qSortName larger)
--   where smaller = [y | y <- xs, fst3(y) <= fst3(x)]
--         larger =  [y | y <- xs, fst3(y) > fst3(x)]

qSortName' :: (Ord a) => (a -> a -> Ordering) -> [(a,b,c)] -> [(a,b,c)]
qSortName' _ []      =   []
qSortName' c (x:xs)  =   (qSortName' c smaller ) ++ (x : equal) ++ (qSortName' c larger)
    where 
        smaller  =  [y | y <- xs, fst3(y) `c` fst3(x) == LT]
        larger   =  [y | y <- xs, fst3(y) `c` fst3(x) == GT]
        equal    =  [y | y <- xs, fst3(y) `c` fst3(x) == EQ]


{-- that sorts a spreadsheet lexicographically
by names using an higher order version of the quick sort algorithm.
--}


{--------- 1b (10 marks) -----------------------------------

Write a function--}


sortUsername :: Spreadsheet -> Spreadsheet
sortUsername sp = qSortUsername comp' sp

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- qSortUsername :: Ord b => [(a,b,c)] -> [(a,b,c)]
-- qSortUsername [] = []
-- qSortUsername (x:xs) = (qSortUsername smaller) ++ [x] ++ (qSortUsername larger)
--   where smaller = [y | y <- xs, snd3(y) <= snd3(x)]
--         larger =  [y | y <- xs, snd3(y) > snd3(x)]


qSortUsername :: (Ord b) => (b -> b -> Ordering) -> [(a,b,c)] -> [(a,b,c)]
qSortUsername _ []      =   []
qSortUsername c (x:xs)  =   (qSortUsername c smaller ) ++ (x : equal) ++ (qSortUsername c larger)
    where 
        smaller  =  [y | y <- xs, snd3(y) `c` snd3(x) == LT]
        larger   =  [y | y <- xs, snd3(y) `c` snd3(x) == GT]
        equal    =  [y | y <- xs, snd3(y) `c` snd3(x) == EQ]


{-- that sorts a spreadsheet lexicographically
by username using an higher order version of the quick sort algorithm.
--}


{--------- 1c (10 marks) -----------------------------------

Write a function--}


sortMark :: Spreadsheet -> Spreadsheet
sortMark sp = qSortMark comp' sp

thrd3 :: (a, b, c) -> c
thrd3 (_, _, x) = x

-- qSortMark :: Ord c => [(a,b,c)] -> [(a,b,c)]
-- qSortMark [] = []
-- qSortMark (x:xs) = (qSortMark smaller) ++ [x] ++ (qSortMark larger)
--   where smaller = [y | y <- xs, thrd3(y) <= thrd3(x)]
--         larger =  [y | y <- xs, thrd3(y) > thrd3(x)]

qSortMark :: (Ord c) => (c -> c -> Ordering) -> [(a,b,c)] -> [(a,b,c)]
qSortMark _ []      =   []
qSortMark c (x:xs)  =   (qSortMark c smaller ) ++ (x : equal) ++ (qSortMark c larger)
    where 
        smaller  =  [y | y <- xs, thrd3(y) `c` thrd3(x) == LT]
        larger   =  [y | y <- xs, thrd3(y) `c` thrd3(x) == GT]
        equal    =  [y | y <- xs, thrd3(y) `c` thrd3(x) == EQ]


{-- that sorts a spreadsheet lexicographically
by mark using an higher order version of the quick sort algorithm.
--}



--------------------------------------------------------------------
----- EXERCISE 2
---------------------------------------------------------------------


{-- We want to print chessboards of various sizes. For instance a 4x4 board:

printBoard 4 =

.----------------.
|    ****    ****|
|    ****    ****|
|****    ****    |
|****    ****    |
|    ****    ****|
|    ****    ****|
|****    ****    |
|****    ****    |
.----------------.

We think of a chessboard as a big tile


    ****    ****
    ****    ****
****    ****    
****    ****    
    ****    ****
    ****    ****
****    ****    
****    ****

consisting of smaller black and white tiles.

The big tile is surrounded by a decorative edge.

.----------------.
|                |
|                |
|                |
|                |
|                |
|                |
|                |
|                |
.----------------. 


We will produce a "black" tile like

****
****

by pretty printing the  list of strings

["****","****"] (4 stars)


We will produce a "white" square by pretty printing the  list of strings

["    ","    "] (4 blanks each)


Both a tile and the whole board are made from characters.
They are lists of list of characters, to be precise.


At the same time, it is useful to think of a chessboard without its edge
as a tile consisting of black and white tiles.

Hence we introduce a general polymorphic type of tiles--}


type Tile a = [[a]]


{-- 2a (10 marks) -------------------------------------------

We begin with single coloured tiles.
Write a function:
--}


makeTile :: Int -> Char -> Tile Char
makeTile n c = [replicate n a | a <- [c] , _ <- [1..n`div`2]]


{--that given a character c and an integer n produces a tile
consisting of  n`div`2  lines of n characters c.

For example:

makeTile ’*’ 4 = ["****","****"]
makeTile ’*’ 5 =["*****","*****"]

Hint: use list comprehension.
--}


{-- 2b (10 marks) -------------------------------------------

In order to print a tile of characters (an element of Tile Char)
write down a function
--}


putTileChr :: Tile Char -> IO()
putTileChr [] = return()
putTileChr (x:xs) = do putStr x
                       putChar '\n'
                       putTileChr xs


{-- for example:

Worksheet6> putTileChr ["****","****"]
****
****

This same function can also be used to print a board when it is given
to us as a list of strings.

----------

In order to build a chessboard it is convenient to think of it as a
tile of tiles of characters. To print an element of Tile(Tile Char) we
will transform it first to an element in Tile Char.

We do this in steps.

First we glue two tiles together: this can be done horizontally and
vertically horizontally as well as vertically as suggested by the
following glueing functions:

&&&  ***      &&&***
&&&  ***  to  &&&***  (horizontal glueing)

&&&    
&&&       &&&
          &&&
***       ***
***  to   ***         (vertical glueing)



-- 2c (10 marks) ----------------------

Write a function--}


hglue :: Tile a -> Tile a -> Tile a
hglue xs [] = xs
hglue [] ys = ys
hglue (x:xs) (y:ys) =  [x ++ y] ++ (hglue xs ys)

-- hglue' :: Tile a -> Tile a
-- hglue' list = unpack' (map (concat) list)
-- hglue' [] = []
-- hglue' (x:xs) = (combine x) ++ (hglue' xs)

-- putToList :: a -> [a]
-- putToList list = list : []

-- combine :: [[a]] -> [a]
-- combine list = myconcat list

unpack' ::  [a] -> Tile a
--unpack list = (map (concat) list)
unpack' list = list : []
-- unpack' [] = []
-- unpack' (x:xs) = [x] ++ (unpack' xs)



-- myConcat :: Tile a -> Tile a
-- myConcat [] = []
--myConcat list = [intercalate' "" list]

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ (myconcat xs)



join :: [a] -> [[a]] -> [a]
join s [] = []
join s [x] = x
join s (x:xs) = x ++ s ++ (join s xs)

-- merge' :: [[a]] -> [a]
-- merge' list = [list >>= id]



{-- that glues two tiles horizontally as suggested by:

&&&  ***      &&&***
&&&  ***  to  &&&***  (horizontal glueing)

you may assume they have the same size. --}



-- 2d (10 marks) ----------------------

--And write a function--}


vglue :: Tile a -> Tile a -> Tile a
--vglue a b = a ++ b
vglue xs [] = xs
vglue [] ys = ys
vglue (x:xs) (y:ys) =   x : (vglue xs ys) ++ [y]

{-- that glues two tiles vertically as suggested by

&&&    
&&&       &&&
          &&&
***       ***
***  to   ***   (vertical glueing)

(you may assume they have the same size) --}



{-- 2e (10 marks) ----------------------

Write a function--}


row2tile :: [Tile a] -> Tile a
row2tile [] = []
--row2tile (x:xs) = hglue x (row2tile xs)
row2tile (x:xs) = hglue x (row2tile xs)

-- row2tile' :: Tile a -> Tile a
-- row2tile' (x:xs) = (myConcat x) ++ 

{--that will glue a row of tiles horizontally to one long tile.
[["&&&","&&&"],["***","***"], ["&&&","&&&"]]

-- 2f (10 points) ----------------------

Write a function--}


col2tile  :: [Tile a] -> Tile a
col2tile [] = []
col2tile (x:xs) = vglue x (col2tile xs)


{--that will glue a column of tiles vertically to one tall tile.



-- 2g (10 points) ---------------

So, if we think of a board as a column of rows of tiles, then we can
convert a board into a tile using col2tile and row2tile.

Now write a function--}


flattenTile :: Tile (Tile Char) -> Tile Char
flattenTile list = unpack(map (concat) list)
-- flattenTile [] = []
-- flattenTile (x:xs) = (row2tile x) ++ (flattenTile xs)

unpack ::  [[[Char]]] -> Tile Char
--unpack list = (map (concat) list)
unpack [] = []
unpack (x:xs) = [join "" x] ++ (unpack xs)

{-- that transforms tile of tiles into one big tile:
For instance if we would flatten the 2x2 chessboard

[[["  "],["**"]],[["**"],["  "]]] in Tile(Tile Char)

[[["  "],["**"]],[["**"],["  "]],[["  "],["**"]],[["**"],["  "]]]

we should get

["  **","**  "] in Tile Char.


-- 2h (10 points) ---------------

Next we want to construct an edge around a tile of characters
with a function.--}

addEdge :: Tile Char -> Tile Char
addEdge list = addDot (map ("|"++) (map (++ "|") list))
-- addEdge list = addDot (map ("|"++) (map (++ "|") list))

addDot :: Tile Char -> Tile Char
addDot list = ["." ++ replicate (wordLength list) '-' ++ "."| '-' <- ['-'] , _ <- [1]] ++ list ++ [ "." ++ replicate (wordLength list) '-'  ++ "." | '-' <- ['-'] , _ <- [1]]
-- addDot list = [".----."] ++ list ++ [".----."]
-- addDot  [] = [".----."]
-- addDot (x:xs) = x : addDot  xs

underline :: Int -> String
underline n = replicate n '-'

wordLength :: Tile Char -> Int
wordLength (x:xs) =  length x - 2




{-- For example edge should transform the tile

["****","****"]

in the tile

[".----.","|****|","|****|",".----."]

which should print with help of the earlier putTileChr to

.----.
|****|
|****|
.----.

The chessboard shown at the beginning of this question is another example.--}


{-- 2 i (20 points) ----------------------


Finally write a function--}


printBoard :: Int -> IO()
--printBoard n = putTileChr(addEdge((makeTile n ' ') ++ (makeTile n '*')))
--printBoard n = putTileChr(printBoard' ((makeTile n ' ') ++ (makeTile n '*')))
--printBoard n = printBoard'(makeTile n ' ') (makeTile n '*')
printBoard n =  putTileChr(addEdge(flattenTile(printBoard' (hglue(makeTile n ' ') (makeTile n '*')) (hglue (makeTile n '*') (makeTile n ' ')))))

printBoard' :: Tile Char -> Tile Char -> [[Tile Char]]
--printBoard' list = replicate 4 list
--printBoard' black white = addEdge(col2tile (replicate 4 (black++white)))
printBoard' black white = (replicate 4 ([black]++[white]))

-- tilePrint :: [Tile Char] -> IO()
-- tilePrint [] = return ()
-- tilePrint (x:xs) = do
--     putTileChr(x) 
--     tilePrint xs

 

{--

that given a number produces a chessboard of that size with the
black square in the BOTTOM LEFT.

The output should resemble (using ghci my prompt is Worksheet6)

Worksheet6>printBoard 3 
.---------.
|***   ***|
|***   ***|
|   ***   |
|   ***   |
|***   ***|
|***   ***|
.---------.

Worksheet6>printBoard 4
.----------------.
|    ****    ****|
|    ****    ****|
|****    ****    |
|****    ****    |
|    ****    ****|
|    ****    ****|
|****    ****    |
|****    ****    |
.----------------.

Worksheet6>printBoard 8
.----------------------------------------------------------------.
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|        ********        ********        ********        ********|
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
|********        ********        ********        ********        |
.----------------------------------------------------------------.

--}
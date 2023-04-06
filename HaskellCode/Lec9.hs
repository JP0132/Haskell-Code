module Lec10 where
import Data.Char
--A enumerated type
data Temp = Cold | Hot deriving (Eq,Show)
--Temp is a type
--Cold and Hot are constructors
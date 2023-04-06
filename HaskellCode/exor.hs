import Data.Char

exOr :: Bool -> Bool -> Bool
exOr True True = False
exOr False True = True
exOr True False = True
exOr False False = False


exOr2 p q = (not (p && q)) && (p || q)
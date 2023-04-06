winUpdate :: (String, Int) -> (String, Int)
winUpdate (x,y) = (x,y+3)

movePoint :: Int -> Int -> (Int,Int) -> (Int,Int)
movePoint m n (x,y) = (x+m, y+n)
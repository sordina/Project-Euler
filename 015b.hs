import Data.Array

tabulate bounds f = array bounds [(i,f i) | i <- range bounds]
dp bounds f = (memo!) where memo = tabulate bounds (f (memo!))
 
p15 n = dp ((0,0),(n,n)) f (n,n) where
 f rec (x,y) | x == 0 || y == 0 = 1
             | otherwise = rec (x-1,y) + rec (x,y-1)
 
main = print $ p15 20

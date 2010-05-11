import Data.List
 
nums = [ n | n <- [3,5..], n `mod` 5 /= 0 ]
 
period n =
    head $ [ p | p <- [1..], (10^p - 1) `mod` n == 0 ]
 
answer =
    fst $
    maximumBy (\(_,a) (_,b) -> compare a b) $
    map (\n -> (n,period n)) $
    takeWhile (<1000) nums

--- Libraries

import Control.Monad
import Data.List

--- Problem

solution1 = permute'' ['0'..'9'] !! (10^6 - 1)

--- Definitions

permute''' :: [a] -> [[a]]
permute''' [] = [[]]
permute''' xs = [x:zs | (x,ys) <- expose xs, zs <- permute''' ys]
             where expose xs = step [] xs
                   step _  []     = []
                   step ys (x:xs) = (x,ys++xs):step (ys++[x]) xs

permute'' :: Eq a => [a] -> [[a]]
permute'' [] = [[]]
permute'' str = do
   x  <- str
   xs <- permute'' (delete x str)
   return (x:xs)

-- Nieve implementation
permute :: (Eq a, Ord a) => [a] -> [[a]]
permute l = filter (not . duplicates) $ replicateM (length l) l

duplicates :: (Eq a, Ord a) => [a] -> Bool
duplicates = (> 1) . last . sort . map length . group . sort

put :: Int -> a -> [a] -> [a]
put 0 x l      = x : l
put _ x []     = x : []
put i x (h:t)  = h : put (i-1) x t


selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

permutations' :: [a] -> [[a]]
permutations' xs =
   [ y : zs
   | (y,ys) <- selections xs
   , zs     <- permutations' ys
   ]

--- Plumbing

main :: IO ()
main = print solution1

--- Properties

prop_permute = permute "012" == ["012","021","102","120","201","210"]
prop_permute'' = permute'' "abcde" == permute "abcde"

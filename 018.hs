----------------
{- Problem 18 -}
----------------

import Control.Monad (liftM)
import Data.List (init)

main = mapM_ (print . solve =<<) [t1,t2]

solve = foldr1 (\x y -> zipWith (+) x (f y))
  where
    f = (\ xxs@(x:xs) -> zipWith max xxs xs)

-- File IO
t1 = triangleFile' "018_t1.txt"
t2 = triangleFile' "018_t2.txt"

triangleFile' = liftM triangle' . readFile

triangle' :: String -> Triangle' Integer
triangle' = map (map read . words) . lines

-- Types
type Triangle' a = [[a]]

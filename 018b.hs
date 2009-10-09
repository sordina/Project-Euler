----------------
{- Problem 18 -}
----------------

import Control.Monad (liftM)
import Data.List (init)

main = mapM_ (print . solve . triangle =<<) [t1,t2]

-- Brute force!
solve :: Triangle Integer -> Integer
solve Empty = 0
solve t = max tl tr + h
  where
    h = tip t
    tl = solve $ left t
    tr = solve $ right t

-- Types
type Triangle' a = [[a]]

data Triangle a =
  Empty |
  Triangle { tip :: a, left :: Triangle a, right :: Triangle a }
  deriving Show

-- Constructor functions
triangle :: Triangle' a -> Triangle a
triangle [] = Empty
triangle t = Triangle {
    tip = head (head t),
    left = cropLeft t,
    right = cropRight t
  }
  where
    cropLeft, cropRight :: Triangle' a -> Triangle a
    cropLeft  = triangle . map init . tail
    cropRight = triangle . map tail . tail

-- File IO
t1 = triangleFile' "018_t1.txt"
t2 = triangleFile' "018_t2.txt"

triangleFile' = liftM triangle' . readFile

triangle' :: String -> Triangle' Integer
triangle' = map (map read . words) . lines

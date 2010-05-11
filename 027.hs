{-# LANGUAGE BangPatterns #-}

import Primes
import Data.List (maximumBy)

formula :: Integer -> Integer -> Integer -> Integer
formula a b = \n -> (n^2) + (a*n) + b

prop_formula_a = 40 == nPrimes euler
  where euler = map (formula 1 41) [0..]

prop_formula_b = 80 == nPrimes computer
  where computer = map (formula 79 1601) [0..]

primeMakers :: [(Integer,Integer,Integer)]
primeMakers = do
  a <- r; b <- r
  let set = map (formula a b) [0..]
  return $ (a,b, nPrimes set) 

  where
    r :: [Integer]
    r =  [-999 .. 999]

nPrimes :: Integral a => [a] -> Integer
nPrimes = nPrimes' primes

-- Bang to avoid a stack overflow.
nPrimes' :: (Eq a, Ord a) => [a] -> [a] -> Integer
nPrimes' [] _ = 0
nPrimes' _ [] = 0
nPrimes' (h:t) ! o@(x:xs)
  | h <  x    = nPrimes' t o
  | h == x    = 1 + nPrimes' t xs
  | otherwise = 0

answer :: (Integer,Integer,Integer)
answer = maximumBy f primeMakers
  where f (_,_,n1) (_,_,n2) = n1 `compare` n2

main :: IO ()
main =  print answer

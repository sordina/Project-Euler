module Main where

import Data.List (foldl')
import Control.Parallel (par,pseq)

main = print $ l `par` r `pseq` result
  where
    result = myMaximum $ l ++ r
    l = go 1 $ n5 - 1
    r = go n5 n6
    n5 = 10^5
    n6 = 10^6

go start finish = map numeric [start..finish]

myMaximum :: [Integer] -> Integer
myMaximum list =
  let (_,(answer,_)) = foldl' f (0,(0,0)) list
  in answer
  where
    f (i, old@(_,best)) n
      | n > best = (succ i, (succ i, n))
      | otherwise = (succ i, old)

-- Numeric version of f
--
numeric :: Integer -> Integer
numeric x = f 1 x
	where
		f :: Integer -> Integer -> Integer
		f acc 1 = acc
		f acc n
			| even n    = f acc' $ n `div `2
			| otherwise = f acc' $ 3 * n + 1
			where
				acc' = acc + 1

prop_numeric :: Integer -> Bool
prop_numeric n
	| n < 1 = True
	| n > 100000 = True
	| otherwise = numeric n == fromIntegral $ length (naieve n)

-- Naieve version
--
naieve :: Integer -> [Integer]
naieve 1 = [1]
naieve n
  | even n    = n' : naieve (n `div `2)
  | otherwise = n' : naieve (3 * n + 1)
  where
    n' = fromIntegral n

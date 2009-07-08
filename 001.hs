{---
 - Sum the natural numbers below 1000
 - that are a multiple of 3, or 5.
 ---}

module Main (main) where
import Data.Set

main :: IO ()
main = mapM_ (putStrLn . show) [
		sum_three_fives 999,
		sum2 3 5 999,
		sum [x | x <- [1 .. 999], x `mod` 3 == 0 || x `mod` 5 == 0],
		sum [3,6..999] + sum [5,10..999] - sum [15,30..999]
	]

-- Implementation 1 -- Generally pretty shit (doh)
--
sum_three_fives :: Integer -> Integer
sum_three_fives n = sum $ toList $ threes_and_fives n
	where
		threes, fives :: Integer -> Set Integer

		threes n = fromAscList [0,3..n]
		fives  n = fromAscList [0,5..n]

		threes_and_fives :: Integer -> Set Integer
		threes_and_fives n = union (threes n) (fives n)

-- Implementation 2 -- Constant time (woot)
--
sum2 :: Integer -> Integer -> Integer -> Integer
sum2 a b ceiling = aX + bX - abX
	where
		aX  = sum1 a ceiling
		bX  = sum1 b ceiling
		abX = sum1 (a * b) ceiling

		sum1 :: Integer -> Integer -> Integer
		sum1 x ceiling = sum1' (even times) times x
			where
				times = ceiling `div` x

		sum1' :: Bool -> Integer -> Integer -> Integer
		sum1' True times x = area
			where
				area = (times + 1) * (times * x) `div` 2

		sum1' False times x = max + area'
			where
				max   = times * x
				area' = sum1' True (times - 1) x

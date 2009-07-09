{---
 - Problem:
 - Find the smallest number where
 - [2..20] all divide number
 ---}

module Main (main)
where
import Prime (primeFactors)
import Data.Set (empty, fromList, toList, union, fold, insert, Set)

-- main1 is too slow
--
funs = [main3]
prop_funs = funs ++ [main1]

main = mapM_ (putStrLn . show) $
	map (\f -> f 20) funs

-- property as given in problem description
--
prop_main = all (\f -> f 10 == 2520) prop_funs

-- Brutest force - Too slow
--
main1 top = head [x| x <- [top .. high], all (\e -> x `mod` e == 0) set1]
	where
		set1 = [2 .. top]
		high = product set1

-- Primes - Takes a product of the prime factors set.
-- Flawed idea - Doesn't take into account composite composition
--
main2 top = product $ toList $ llSet $ pfList [2 .. top]
	where
		llSet :: [[Integer]] -> Set Integer
		llSet l = foldl folder empty l
		folder = \x y -> union x (fromList y)
		pfList l = map primeFactors l

-- Primes - Takes a product of the non factor elements
-- Flawed idea - *shrug*
--
main3 :: Integer -> Integer
main3 top = product ( toList ( removeFactors ( fromList [2 .. top] ) ) )
	where
		removeFactors :: Set Integer -> Set Integer
		removeFactors l = fold folder empty l

		folder :: Integer -> Set Integer -> Set Integer
		folder e a = if (isUnique e a) then (insert e a) else a

		isUnique :: Integer -> Set Integer -> Bool
		isUnique e s = all (\ea -> ea `mod` e /= 0) $ toList s

-- Libraries
--
sqrti = truncate . sqrt . fromIntegral

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
funs = [main1b, main3]
prop_funs = funs ++ [main1, main1a]

main = mapM_ (putStrLn . show) $
	map (\f -> f 20) funs

-- property as given in problem description
--
prop_main = all (\f -> f 10 == 2520) prop_funs

-- Brutest force - Too slow - But is correct
--
main1 top = head [ x | x <- [top .. high], all (\e -> x `mod` e == 0) set1]
	where
		set1 = [2 .. top]
		high = product set1

-- Brute force - Too slow - filters based on top interval
--
main1a top = head [ x | x <- [top, top * 2 .. high], all (\e -> x `mod` e == 0) set1]
	where
		set1 = [2 .. top]
		high = product set1

-- Brute force - Too slow - filters based on prime-factor interval
--
main1b top = head [ x | x <- [pfi, pfi * 2 .. high], all (\e -> x `mod` e == 0) set1]
	where
		set1 = [2 .. top]
		high = product set1
		pfi  = main2 top

-- Primes - Takes a product of the prime factors set.
-- Flawed idea - Doesn't take into account composite composition
-- !!! But can be used for sub computation task
--
main2 top = product $ toList $ llSet $ pfList [2 .. top]
	where
		llSet :: [[Integer]] -> Set Integer
		llSet l = foldl folder empty l
		folder = \x y -> union x (fromList y)
		pfList l = map primeFactors l

-- DUH version
--
main3 top = foldl lcm 1 [2 .. top]

-- Libraries
--
sqrti = truncate . sqrt . fromIntegral

module Main (main)
where
import Data.List (sort)

main :: IO ()
main  = mapM_ (putStrLn . show) $
	map (\f -> f 100 999) [ main1, main2, main3 ]

-- Implementation 1 - Brutest force
--
main1 :: Integer -> Integer -> Integer
main1 low high =
	foldl max 0 [z| z<-[x*y| x<-r, y<-r], let z' = show z in z' == reverse z']
	where r = [low .. high]

-- Implementation 2 - Avoid duplicates
--
main2 :: Integer -> Integer -> Integer
main2 low high =
	foldl max 0 [z| z<-[x*y| x<-r, y<-[x .. high]], let z' = show z in z' == reverse z']
	where r = [low .. high]

-- Implementation 3 - Simple wheel - Better max function
--
main3 :: Integer -> Integer -> Integer
main3 low high =
	maximum [z| z<-[x*y| x<-r, y<-[x .. high]], let z' = show z in z' == reverse z']
	where
		timesLow = low `div` 11
		low' = 11 * timesLow
		r = [low', low' + 11 .. high]

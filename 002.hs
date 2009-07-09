module Main (main)
where

main :: IO ()
main = (putStrLn . show) $ main1 4000000
	
main1 :: Integer -> Integer
main1 cap = sum [x | x <- takeWhile (<= cap) fibs, even x]
	where
		fibs :: [Integer]
		fibs = 0 : 1 : (zipWith (+) fibs $ tail fibs)

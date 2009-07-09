{- prime reference: http://www.haskell.org/haskellwiki/Prime_numbers -}

module Main (main)
where

main :: IO ()
main = mapM_ (putStrLn . show) $
	map (\x -> x 600851475143) [ main1, main2 ]

main1 :: Integer -> Integer
main1 x = p $ sqrti x
	where
		p :: Integer -> Integer
		p 1 = 1
		p y | x `mod` y == 0 && prime y = y
				| otherwise = p $ y - 1

		prime :: Integer -> Bool
		prime 1 = True
		prime z = prime' 2 (sqrti z) z

		prime' x y z
			| x > y = False
			| z `mod` x == 0 = False
			| x == y = True
			| otherwise = prime' (x + 1) y z

		sqrti :: Integer -> Integer
		sqrti = truncate . sqrt . fromIntegral

main2 :: Integer -> Integer
main2 x = last $ primeFactors x

isPrime n = n > 1 && n == head (primeFactors n)
 
primeFactors 1 = []
primeFactors n = go n primes
	 where
	 go n ps@(p:pt)
			| p*p > n        = [n]
			| n `rem` p == 0 = p : go (n `quot` p) ps
			| otherwise      = go n pt

primes :: [Integer]
primes = 2:3:go 5 [] (tail primes)
	where
		divisibleBy d n = n `mod` d == 0
		go start ds (p:ps) = let pSq = p*p in
				foldr (\d -> filter (not . divisibleBy d)) [start, start + 2..pSq - 2] ds
				++ go (pSq + 2) (p:ds) ps

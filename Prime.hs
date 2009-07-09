{- prime reference: http://www.haskell.org/haskellwiki/Prime_numbers -}

module Prime (isPrime, primeFactors, primes)
where

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

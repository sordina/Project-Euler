{- prime reference: http://www.haskell.org/haskellwiki/Prime_numbers -}

module Prime (isPrime, primeFactors, primes)
where

isPrime n = n > 1 && n == head (primeFactors n)
 
primeFactors 1 = []
primeFactors n = go n primes_
	 where
	 go n ps@(p:pt)
			| p*p > n        = [n]
			| n `rem` p == 0 = p : go (n `quot` p) ps
			| otherwise      = go n pt

primes_ :: [Integer]
primes_ = 2:3:go 5 [] (tail primes_)
	where
		divisibleBy d n = n `mod` d == 0
		go start ds (p:ps) = let pSq = p*p in
				foldr (\d -> filter (not . divisibleBy d)) [start, start + 2..pSq - 2] ds
				++ go (pSq + 2) (p:ds) ps

-- Sieve of Eratosthenes
--

data People a = VIP a (People a) | Crowd [a]
 
mergeP :: Ord a => People a -> People a -> People a
mergeP (VIP x xt) ys                    = VIP x $ mergeP xt ys
mergeP (Crowd xs) (Crowd ys)            = Crowd $ merge  xs ys
mergeP xs@(Crowd ~(x:xt)) ys@(VIP y yt) = case compare x y of
    LT -> VIP x $ mergeP (Crowd xt) ys
    EQ -> VIP x $ mergeP (Crowd xt) yt
    GT -> VIP y $ mergeP xs yt
 
 
merge :: Ord a => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = case compare x y of
    LT -> x : merge xt ys
    EQ -> x : merge xt yt
    GT -> y : merge xs yt
 
diff xs@(x:xt) ys@(y:yt) = case compare x y of
    LT -> x : diff xt ys
    EQ ->     diff xt yt
    GT ->     diff xs yt
 
foldTree :: (a -> a -> a) -> [a] -> a
foldTree f ~(x:xs) = f x . foldTree f . pairs $ xs
    where pairs ~(x: ~(y:ys)) = f x y : pairs ys
 
primes, nonprimes :: [Integer]
primes    = 2:3:diff [5,7..] nonprimes
nonprimes = serve . foldTree mergeP . map multiples $ tail primes
    where
    multiples p = vip [p*k | k <- [p,p+2..]]
 
    vip (x:xs)       = VIP x $ Crowd xs
    serve (VIP x xs) = x:serve xs
    serve (Crowd xs) = xs

-- Slooooow, but quite simplistic
primesWheel :: [Integer]
primesWheel = primeSeq 2 []
  where
    primeSeq u ps
      | all (\e -> u `mod` e /= 0) ps = u : (primeSeq u' $ ps ++ [u])
      | otherwise = primeSeq u' ps
      where
        u' = u + 1

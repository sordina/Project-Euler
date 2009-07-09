{---
 - A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 - a^(2) + b^(2) = c^(2)
 -
 - For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
 -
 - There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 - Find the product abc.
 ---}

module Main (main)
where

main :: IO ()
main  = mapM_ print [main3, main4]

-- Brutest force
-- Fail: Way too slow
--
main1 :: Integer
main1 = head [a*b*c| a<-l, b<-l, c<-l, a^2 + b^2 == c^2, a+b+c == 1000]
	where l = [1..1000]

-- Brute sieve
-- Fail: Too slow
--
main2 :: Integer
main2 = head [a*b*c| (a,b,c) <- triples, a+b+c == 1000]
	where triples = [(a,b,c)| a<-l, b<-l, c<-l, a^2 + b^2 == c^2]
		where l = [1..1000]

-- Simple sieve
--
main3 :: Integer
main3 = head [a*b*c| (a,b,c) <- triples, a+b+c == 1000]
	where triples = [(a, b, c)| b<-l, c<-l, c2 <- [b^2 + c^2], a <- [sqrti c2], a ^ 2 == c2 ]
		where
			l = [1..1000]
			sqrti = truncate . sqrt . fromIntegral 

-- Better sieve
--
main4 :: Integer
main4 = head [a*b*c| (a,b,c) <- triples, a+b+c == 1000]
	where triples = [(a, b, c)| b<-l 1, c<-l b, c2 <- [b^2 + c^2], a <- [sqrti c2], a ^ 2 == c2 ]
		where
			l b = [b..1000]
			sqrti = truncate . sqrt . fromIntegral 

{---
 -
-- Linear time (Piere)
--
main5 :: Integer
main5 = product $ find_triplet 1000
	where
		find_triplet :: Integer -> [Integer]
		find_triplet sum = [2*m*n, m^2 - n^2, m^2 + n^2]
			where
				(m, n) = head [ (if b1 then m1 else m2, n) |
						n <- [0..sum],
						delta <- [n^2 + 2*sum],
						m1 <- [sqrt delta - n / 2],
						m2 <- [0 - (sqrt delta + n / 2)],
						b1 <- [isInteger m1 && m1 > n],
						b2 <- [isInteger m2 && m2 > n],
						b1 || b2
					]

isInteger x = truncate x == x
---}

import Memoization

naieve :: Integer -> Integer
naieve = f
	where
		f 0 = 1
		f 1 = 1
		f n = (f (n-1)) + (f (n-2))

f0 :: [Integer]
f0 = map naieve [0..]

f1 :: [Integer]
f1 = 1 : 1 : zipWith (+) f1 (tail f1)

f2 :: [Integer]
f2 = map f' [0..]
	where
		f' =
			let
				f 0 = 1
				f 1 = 1
				f n = (f' (n-1)) + (f' (n-2))
			in
				(map f [0..] !!)

f3 :: [Integer]
f3 = map (memoizeInt naieve) [0..]

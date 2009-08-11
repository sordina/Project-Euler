import qualified Data.List as D

main = print first

triangles :: [Integer]
triangles = scanl (+) 0 [1..]

(Just first) = D.find ((>500).nFactors) triangles

{---
 -
 - The nFactors function is wrong for numbers < 4
 - Unsure why.
 -
 ---}
 --
nFactors :: Integer -> Integer
nFactors 0 = 1
nFactors x =
	if sqrt ^2 == x
	then fromIntegral len * 2 + 1
	else fromIntegral len * 2
	where
		len = factDown sqrt' 0
		sqrt = sqrti x
		sqrt' = sqrt - 1
		factDown 0 y = y
		factDown n y
			| x `mod` n == 0 = factDown (n-1) (y+1)
			| otherwise      = factDown (n-1)  y

sqrti :: Integer -> Integer
sqrti = truncate . sqrt . fromInteger

choose :: Integer -> Integer -> Integer
choose m 0 = 1
choose 0 n = 0
choose (m+1) (n+1) = (choose m n) * (m+1) `div` (n+1)

choose :: Integer -> Integer -> Integer
choose _ 0 = 1
choose 0 _ = 0
choose (m+1) (n+1) = (choose m n) * (m+1) `div` (n+1)

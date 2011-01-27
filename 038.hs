-- Note: this can be hevily optimized. At the moment it is crap.

import Data.List

lhs = [2..9999]
rhs = [[1..n] | n <- [2..9]]

nums :: [Integer]
nums = concatMap f lhs
  where
    f n = map g rhs
      where
        g l = cm $ map (*n) l

cm = foldl1' cp

cp x y = (x*10^(ln y)) + y

ln n
  | n < 10    = 1
  | otherwise = 1 + ln (n `div` 10)

pans = filter pandigital nums

answer :: Integer
answer = maximum pans

pandigital :: Integer -> Bool
pandigital n = n >= 10^8 && n < 10^9 && sort (show n) == "123456789"

main = print answer

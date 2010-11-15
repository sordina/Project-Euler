import Data.List

lhs = [2..10^9-1]
rhs = [[1..n]|n<-[2..9]]

nums :: [Integer]
nums = concatMap f lhs
  where
    f n = map g rhs
      where
        g l = sum $ map (*n) l

answer :: Integer
answer = maximum $ filter pandigital nums

pandigital :: Integer -> Bool
pandigital n = length s == 9 && sort s == "123456789"
  where
    s = show n

main = print answer

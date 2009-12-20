divisors n = [x | x <- [1..(n-1)], n `mod` x == 0]
 
amicable n = let p = sum (divisors n) in
  p /= n && p < 10000 && n == sum (divisors p)

main = print $ sum [n | n <- [1..9999], amicable n]

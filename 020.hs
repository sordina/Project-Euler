main = print answer

answer = sum numbers 
  where
    numbers = map reader digits
    reader = read . (:[])
    digits = show factorial
    factorial = fact 100

fact n = product [1..n]

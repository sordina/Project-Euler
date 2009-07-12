module Main (main)
where
import Prime

main :: IO ()
main = mapM_ (\f -> print $ f 2000000) [prime1]

prime1 :: Integer -> Integer
prime1 upper = sum $ takeWhile (\p -> p < upper) primes

import Data.Numbers.Primes
import Safe
import Data.List

isPandigital :: Integer -> Bool
isPandigital n = (take l c ==) . sort . show $ n
  where
    s = show n
    l = length s
    c = ['1'..'9']

answer = lastMay . filter isPandigital . takeWhile (<10^9) $ primes

main = print answer

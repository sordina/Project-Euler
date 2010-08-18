import Primes (primes)
import Prime  (isPrime)
import Data.List

main = print answer

answer = length circularPrimes

circularPrimes = filter circularPrime . take 999999 $ primes

-- No need to check the prime twice, so drop the first and last numbers
circularPrime :: Int -> Bool
circularPrime = all (isPrime . fromIntegral) . init . tail . rotate

rotate :: Int -> [Int]
rotate n = map number $ zipWith (++) (tails d) (inits d)
  where
    d = digits n

-- Reverses digit order, but this is okay as number interprets in reverse order
digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = y : digits x
    where (x,y) = n `divMod` 10

number :: [Int] -> Int
number = sum . zipWith (*) powers

powers = iterate (*10) 1

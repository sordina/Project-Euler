import Primes (primes)
import Prime (isPrime)
import Base
import Data.List

digits = toBase 10
unDigits = fromBase 10

main = print answer
answer = sum . take 11 $ trunkPrimes
trunkPrimes = filter trunkable . dropWhile (< 10) $ primes
trunkable x = f (tail . init . tails) && f (tail . inits)
  where
    f t = all (isPrime . unDigits) . t . digits $ x

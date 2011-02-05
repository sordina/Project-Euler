import Data.Numbers.Primes
import Data.List
import Data.Function

pf = nub . primeFactors

l = groupBy ((==) `on` (==4) . length . pf) [1..]

answer = head . filter ((==4).length) . filter ((==4) . length . pf . head) $ l

main = print answer

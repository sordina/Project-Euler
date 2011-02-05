import Data.Numbers.Primes
import Data.List
import Data.Function

main =
  print                           .  
  sum                             .
  maximumBy (compare `on` length) .
  filter (isPrime . sum)          .
  concatMap (takeWhile (l . sum)) .
  map (tail . inits)              .
  tails                           .
  takeWhile l                     $ primes

l = (<10^6)

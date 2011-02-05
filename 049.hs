import Data.List
import Data.Numbers.Primes

answer = map (concatMap show)    .  
  filter (all isPrime)           .
  filter (permutes . map show)   .
  takeWhile ((<10000) . last)    .
  map (take 3 . iterate (+3330)) .
  dropWhile (<1000)              $ primes

permutes l = and $ zipWith arePermutes l (tail l)

arePermutes xs ys = sort xs == sort ys

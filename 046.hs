import Data.Numbers.Primes

main = print $ head answer

answer = filter (not.composible) composites

doubleSquares = map ((2*).(^2)) [1..]

isDS n = (==n) . last . takeWhile (<= n) $ doubleSquares

composites = filter (not.isPrime) [3,5..]

composible n = any (isDS . (n-)) ps where ps = takeWhile (<n) primes

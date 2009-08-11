import List (group)
import Prime (primes)
 
primeFactors 1 = []
primeFactors n = factor : primeFactors (n `div` factor)
    where
        factor   = head (filter evenly primes)
        evenly x = n `rem` x == 0
 
divisorCount = product . map ((1+) . length) . group . primeFactors
 
triangles = 1 : zipWith (+) [2..] triangles
 
solution = fst $ head $ filter ((>500) . snd) $ map (\x -> (x, divisorCount x)) triangles
 
main = print solution

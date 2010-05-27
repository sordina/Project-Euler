import Prime (isPrime)
import Primes
import Data.List (maximumBy)

formula :: Integer -> Integer -> Integer -> Integer
formula a b = \n -> (n^2) + (a*n) + b

primeMakers :: [(Integer,Integer,Integer)]
primeMakers = do
  a <- r; b <- r
  let set = map (formula a b) [0..]
  return $ (a,b, nPrimes set) 

  where
    r :: [Integer]
    r =  [-999 .. 999]

nPrimes :: [Integer] -> Integer
nPrimes = fromIntegral . length . takeWhile (isPrime)

answer :: (Integer,Integer,Integer)
answer = maximumBy f primeMakers
  where f (_,_,n1) (_,_,n2) = n1 `compare` n2

main :: IO ()
main = do
  (x,y,_) <- return answer
  print $ x * y

-- Properties
--
prop_formula_a = 40 == nPrimes euler
  where euler = map (formula 1 41) [0..]

prop_formula_b = 80 == nPrimes computer
  where computer = map (formula (-79) 1601) [0..]

prop_formula_c = and (map isPrime computer)
  where computer = map (formula (-79) 1601) [0..79]

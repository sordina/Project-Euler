import Data.List
import Data.Numbers.Primes
import Safe

-- Should be able to do this without Eq constraint
permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute str = do
  x  <- str
  xs <- permute (delete x str)
  return (x:xs)

num :: Integral a => [a] -> a
num = sum . zipWith (*) powers . reverse

powers :: Integral a => [a]
powers = iterate (*10) 1

-- For some reason, we can disgard pandigitals >= 8.. Investigate further.
answer = headMay . filter isPrime . map num . concatMap permute . initSafe . tails $ [7,6..1]

main = print answer

-- Properties
prop_permute = permute [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

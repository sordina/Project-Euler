import Quadratic
import Data.List
import Data.Function
import Test.QuickCheck

main = print $ abs . uncurry (-) $ minimumBy (compare `on` f) acceptible_pairs
  where
    f (a,b) = abs (a-b)

pentagonal :: Integral n => n -> n
pentagonal n = n * (3*n - 1) `div` 2

isPentagonal n = any (n==) $ map pentagonal (roots n)

-- y = n . (3n-1) / 2
-- 0 = 1.5n^2 - n/2 - y
roots x = filter (>0) . concatMap f $ quadratic (1.5) (0-1/2) (0-y)
  where
    f n = [floor n, ceiling n]
    y   = fromIntegral x

pairs_n n = map f [1..5000]
  where
    f x = (pentagonal x, pentagonal (n+x))

pairs = map pairs_n [1..5000]

prune (x,y) = isPentagonal (x+y) && isPentagonal (abs (y-x))

acceptible_pairs = concatMap (filter prune) pairs

-- Properties

prop_inclusive = forAll (elements (take 100 pentagonals))         isPentagonal
prop_exclusive = forAll (elements (take 100 not_pentagonals)) (un isPentagonal)

-- Helpers

pentagonals = map pentagonal [1..]

simple_pent n = (== n) . last . takeWhile (<=n) $ pentagonals

not_pentagonals = filter (un simple_pent) [1..]

un = (not .)

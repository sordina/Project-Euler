import Control.Monad.Logic
import Data.Set (fromList, member)

main = print answer

max_NAS = 28123

-- Find the sum of all...
answer =
  let ls = fromList low_sums in

    sum $ do

    -- the positive integers...
    -- (It can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.)
    positive <- [1..max_NAS]

    -- which cannot be written as the sum of two abundant numbers.
    guard $ not $ member positive ls

    return positive

low_sums = filter (<= max_NAS) abundant_pair_sums

abundant_pair_sums = do
  x <- abundant_numbers
  y <- abundant_numbers
  return $ x + y

{--- logic version
abundant_pair_sums =
  abundant_numbers >>-
  \x -> abundant_numbers >>-
  \y -> return $ x + y
-}

abundant_numbers = do
  natural <- [0..max_NAS]
  -- A number n is called abundant if the sum of its proper divisors exceeds n.
  guard $ natural < sum (divisors natural)
  return natural

-- As 12 is the smallest abundant number
prop_first_abundant_number = and $ zipWith (==) abb abundant_numbers
  where
    -- Wikipedia: http://en.wikipedia.org/wiki/Abundant_number
    abb = [12, 18, 20, 24, 30, 36, 40, 42, 48, 54, 56, 60, 66, 70, 72, 78, 80, 84, 88, 90, 96, 100]

prop_sums = (12 + 18) `elem` abundant_pair_sums

-- TODO: Optimize
-- Build a list of lists of divisors
divisors number = do
  positive <- [1 .. ceil number]
  guard $ 0 == number `mod` positive
  return positive

ceil = ceiling . (/2) . fromIntegral

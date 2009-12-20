import Control.Monad (guard)

main = print answer

-- Find the sum of all...
answer = sum $ do

  -- the positive integers...
  -- (It can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.)
  positive <- [1..28123]

  -- which cannot be written as the sum of two abundant numbers.
  guard $ not (abundant_pair_sum positive)

  return positive

abundant_pair_sum number =
  not $ null abundant_pairs
  where
    abundant_set1 = takeWhile (< number) abundant_numbers
    abundant_pairs = do
      x <- abundant_set1
      y <- abundant_set1
      guard $ number == x + y
      return x

abundant_numbers = do
  natural <- [0..]
  -- A number n is called abundant if the sum of its proper divisors exceeds n.
  guard $ natural < sum (divisors natural)
  return natural

-- As 12 is the smallest abundant number
prop_first_abundant_number = 12 == head abundant_numbers

-- TODO: Optimize
-- Build a list of lists of divisors
divisors number = do
  positive <- [1 .. ceil number]
  guard $ 0 == number `mod` positive
  return positive

ceil = ceiling . (/2) . fromIntegral

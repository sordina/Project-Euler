import Control.Monad (guard)
import qualified Data.Map as M

range = [1..10000]

d :: Integer -> Integer
d n = sum divisors
  where
    divisors = do
      i <- [1 .. ceil n]
      guard (i `divides` n)
      return i

    denominator `divides` numerator = numerator `mod` denominator == 0

    ceil = ceiling . (/ 2) . fromIntegral

assoc = M.fromList $ zip range (map d [1..])

find = (flip M.lookup) assoc

test :: Integer -> Integer -> Bool
test a b =
  ta && tb
  where
    ta = db == Just a
    tb = da == Just b
    da :: Maybe Integer
    da = find a
    db :: Maybe Integer
    db = find b

amicable = do
  a <- range
  b <- range
  guard $ a /= b
  guard $ test a b
  return a

answer = sum amicable

main = print answer

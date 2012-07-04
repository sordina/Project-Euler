main :: IO ()
main = print answer
  where
    answer    = length valid
    valid     = filter (> 1e6) potential
    potential = [comb n r | n <- range, r <- range]
    range     = [1..100]

comb :: Rational -> Rational -> Rational
comb n r = (fact n) / (fact r * fact (n - r))

fact :: Rational -> Rational
fact n = product [1..n]

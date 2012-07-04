main :: IO ()
main = print answer
  where
    answer    = length valid
    valid     = filter (> 1e6) potential
    potential = [comb n r | n <- [1..100], r <- [1..n]]

comb :: Rational -> Rational -> Rational
comb n r = (fact n) / (fact r * fact (n - r))

fact :: Rational -> Rational
fact n = product [1..n]

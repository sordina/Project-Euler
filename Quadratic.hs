module Quadratic where

quadratic a b c

  | a > 0 && i >= 0 = [l+r,l-r]
  | otherwise       = []

  where
    l  = (0-b) / a2
    r  = sqrt(i) / a2
    i  = b**2 - 4*a*c
    a2 = 2*a

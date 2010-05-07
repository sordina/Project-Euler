module Binomial (binomial)
where

import Factorial
import Control.Arrow ((&&&))

binomial :: Integer -> Integer -> Integer

binomial top bottom = top' `div` bottom'
  where
    top' = factorial top
    bottom' = left * right
      where
        left = factorial bottom
        right = factorial (top - bottom)

main = mapM_ printer list
  where
    printer = print . (uncurry binomial &&& id)
    list = [(top,bottom) | top <- [2..10], bottom <- [2..top]]

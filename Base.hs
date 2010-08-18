module Base (toBase)
where

import Data.List

toBase :: Int -> Int -> [Int]
toBase b v = toBase' [] v where
  toBase' a 0 = a
  toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b

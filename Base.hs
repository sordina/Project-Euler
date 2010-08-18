module Base (toBase, fromBase, toAlphaDigits, fromAlphaDigits)
where

import Data.List
import Data.Char

toBase :: Integral a => a -> a -> [a]
toBase b v = toBase' [] v where
  toBase' a 0 = a
  toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b

fromBase :: Integral a => a -> [a] -> a
fromBase b ds = foldl' (\n k -> n * b + k) 0 ds

toAlphaDigits :: Integral a => [a] -> String
toAlphaDigits = map (convert . fromIntegral) where
  convert n | n < 10    = chr (n + ord '0')
            | otherwise = chr (n + ord 'a' - 10)

fromAlphaDigits :: Integral a => String -> [a]
fromAlphaDigits = map (fromIntegral . convert) where
 convert c | isDigit c = ord c - ord '0'
           | isUpper c = ord c - ord 'A' + 10
           | isLower c = ord c - ord 'a' + 10
           | otherwise = error "Invalid Digit"

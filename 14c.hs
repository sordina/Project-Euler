import Data.Word
import Data.List
 
million = 10^6
 
main = print $ snd $ compute (million-1)
 
compute max = foldl' step (-1,undefined) [1..max]
 
step x i = x `max` (len i,i)
 
len :: Word32 -> Int
len 1                  = 1
len n | n `mod` 2 == 0 = len (n `div` 2) + 1
      | otherwise      = len (3 * n + 1) + 1

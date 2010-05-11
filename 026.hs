import Data.Maybe (fromMaybe)
import Prime

divide :: Integral a => a -> a -> String
divide num den
  | num == 0  = "0"
  | num < den = divide (num*10) den
  | otherwise =
      show quot ++ divide rema den
      where
        quot = num `div` den
        rema = num `rem` den

period :: Integral a => a -> a -> a
period num den = (fromIntegral . subtract 1 . length) $ period' num den []

period' :: Integral a => a -> a -> [a] -> [a]
period' num den prev
  | num `elem` (safeTail prev) = prev
  | num == 0  = []
  | num < den = period' (num*10) den prev
  | otherwise =
      period' rema den (rema:prev)
      where
        quot = num `div` den
        rema = num `rem` den

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

sort' :: Ord b => (a -> b) -> [a] -> a
sort' f = foldl1 folder
  where
    folder o i
      | GT == compare (f o) (f i) = o
      | otherwise = i

answer = sort' (period 1) (takeWhile (<1000) primes)
main = print answer

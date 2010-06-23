import Data.Char (digitToInt)

-- First define the search bounds.

-- Maximum power sum for a particular number of digits.
sMax n = n * 9^5

-- Detemine the minimum number for a number of digits.
nMin n = sum $ map (10^) [0 .. n-1]

-- Used to determine where the max number of digits for the search domain.
cross = c 1
  where
    c n
      | sMax n < nMin n = n
      | otherwise       = c (n+1)

-- Upper limit for the search domain.
limit = sMax cross

numbers :: [Int]
numbers = filter (\n -> n == g n) [2 .. limit]
  where
    g = sum . map ((^5) . digitToInt) . show

answer = sum numbers

main :: IO ()
main = do
  mapM_ print numbers
  print answer

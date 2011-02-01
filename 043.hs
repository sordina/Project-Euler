import Permute
import Data.Numbers.Primes

main = print answer

answer = sum . map mult . filter prop $ pandigitals 9

pandigitals :: Integral n => n -> [[n]]
pandigitals n = filter ((/=0) . head) . permute $ [0..n]

set :: Integral n => [[n]]
set = take 7 $ map (take 3 . flip drop [2..]) [0..]

prop :: Integral n => [n] -> Bool
prop xs = all (==0) divisions
  where
    rows      = map (mult . map ((xs!!) . subtract 1)) set
    divisions = zipWith mod rows primes         

mult :: Integral n => [n] -> n
mult = sum . zipWith (*) (iterate (*10) 1) . reverse

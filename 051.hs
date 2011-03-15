{-# Language TupleSections #-}

import Control.Monad
import Data.List
import Data.Numbers.Primes
import Data.Maybe

main = print $ answer 8

answer :: Int -> Int
answer = head . t3 . fromJust . locate

locate :: Int -> Maybe (Int, Int, [Int])
locate m = find ((==m) . t2) answers

answers :: [(Int, Int, [Int])]
answers = primes >>= f
  where
    f n = [1..numLen n] >>= map numPrimes . (`masks`n)

numPrimes :: (Int, [Int]) -> (Int,Int,[Int])
numPrimes (p, ms) = (p, len, matches)
  where
    start   = if head ms == 0 then 1 else 0
    ds      = digits p
    matches = filter isPrime . map (undigits . \n -> maskAll n ms ds) $ [start..9]
    len     = length matches

-- Helpers

masks :: Int -> Int -> [(Int, [Int])]
masks mask_length prime = map (prime,) $ nonEmptySubSets [0..mask_length - 1]

maskAt :: a -> [a] -> Int -> [a]
maskAt i l n = before ++ [i] ++ after where (before, _:after) = splitAt n l

maskAll :: a -> [Int] -> [a] -> [a]
maskAll mask mask_positions items = foldl' (maskAt mask) items mask_positions 

nonEmptySubSets :: [a] -> [[a]]
nonEmptySubSets = filter (not.null) . filterM (const [True,False])

digits n = f n [] where
  f n
    | n < 10    = (n:)
    | otherwise = let (d,m) = divMod n 10 in f d . (m:)

undigits = sum . zipWith (*) (iterate (*10) 1) . reverse

filterAll :: [a->Bool] -> [a] -> [a]
filterAll [] = id
filterAll fs = foldl1' (.) (map filter fs)

numLen = floor . logBase 10 . fromIntegral

t1 (x,_,_) = x
t2 (_,x,_) = x
t3 (_,_,x) = x

-- properties

prop_digits    = digits    10304                  == [1,0,3,0,4]
prop_mask      = maskAt    'x' "asdf" 1           == "axdf"
prop_maskAll   = maskAll   'x' [1,2,3] "asdfqwer" == "axxxqwer"
prop_undigits  = undigits  [1,2,3,4]              == 1234
prop_filterAll = filterAll [(<3),(>1)] [1,2,3,4]  == [2]
prop_numPrimes = t2 (numPrimes (56003, [2,3]))    == 7
prop_answer    = answer    6                      == 13
prop_answer'   = answer    7                      == 56003

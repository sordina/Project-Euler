----------------
{- Problem 16 -}
----------------

main :: IO ()
main = print $ sumDigits 2 1000

sumDigits :: Integer -> Integer -> Integer
sumDigits base power = sum numbers
  where
    numbers = map (read . (:[])) digits 
    digits = show $ base ^ power

prop_sumDigits :: Bool
prop_sumDigits = a == b
  where
    a = sumDigits 2 15
    b = 26

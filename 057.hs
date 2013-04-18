
import Data.Digits
import Data.Ratio

main :: IO ()
main = print $ length $ filter condition $ take 1000 $ sqrt2

condition :: Rational -> Bool
condition n = length (digits 10 (numerator n)) > length (digits 10 (denominator n))

sqrt2 :: [Rational]
sqrt2 = drop 1 $ map (1+) $ iterate (invert . (2+)) 0
  where
    invert :: Rational -> Rational
    invert x = denominator x % numerator x

tests :: IO ()
tests = mapM_ print [ 1       == (length $ filter condition $ take 8 sqrt2)
                    , (3%2)   == head sqrt2
                    , (7%5)   == (sqrt2 !! 1)
                    , (17%12) == (sqrt2 !! 2)
                    , (41%29) == (sqrt2 !! 3)
                    ]

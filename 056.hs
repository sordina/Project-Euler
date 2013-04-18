
import Data.Digits

main :: IO ()
main = print $ maximum $ map ds [ a^b | a <- r, b <- r] where r = [1..99 :: Integer]

ds :: (Integral n) => n -> n
ds = sum . digits 10

tests :: IO ()
tests = mapM_ print [ 1 == ds (pow 10 100) ]

pow :: Integer -> Integer -> Integer
pow = (^)

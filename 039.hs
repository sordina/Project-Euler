import Data.Function
import Data.List
import Control.Monad

splits :: Integral n => n -> [(n,n,n)]
splits n = do
  c <- [n `div` 3 .. n `div` 2]
  a <- [1..n-c]
  guard $ a /= c
  return (a,n-(c+a),c)

square :: Integral n => (n,n,n) -> Bool
square (a,b,c) = a^2 + b^2 == c^2

perims :: Integral n => n -> [(n,n,n)]
perims = filter square . splits

answer = maximumBy (compare `on` (length . perims)) [5..1000]

main = print answer

-- Certified correct!
--
import Control.Monad
import Control.Arrow
import Data.Ratio
import qualified Data.Set as S
import qualified Data.List as L

main = print answer

answer :: Int
answer = denominator . product $ set

range :: [Int]
range = [10..99]

set :: [Ratio Int]
set = do
  num <- range
  den <- range
  guard $ (den > num) && (acceptable num den)
  return $ num % den

prop_set = length set == 4

acceptable :: Int -> Int -> Bool
acceptable n d = reducable && nontrivial && curious
  where
    curious = cur n d
    reducable = reduce n d /= (n,d)
    nontrivial = divten && matchingdigits
      where
        divten = all ((/= 0) . (`mod` 10)) [n,d]
        matchingdigits = not . S.null $ intersects
        intersects = intersect n d

reduce :: Int -> Int -> (Int,Int)
reduce n d = (numerator &&& denominator) (n%d)

intersect :: Int -> Int -> S.Set Char
intersect n d = foldl1 S.intersection . map (S.fromList . show) $ [n,d]

cur :: Int -> Int -> Bool
cur n d = (n%d) == (strip n % strip d)
  where
    strip :: Int -> Int
    strip = read . L.deleteBy memberTest undefined . show

    int :: S.Set Char
    int = intersect n d

    memberTest :: Char -> Char -> Bool
    memberTest _ x = S.member x int

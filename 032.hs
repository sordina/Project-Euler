-- Broken...
  --
import Data.List
import qualified Data.Map as M

main = print . last . options $ digits

answer = sum products

products = undefined -- nub $ keys

digits :: [Int]
digits = [1..9]

select :: [Int] -> [Int] -> [([Int],[Int])]
select p l = [(e:p, delete e l) | e <- l]

type LL = [([Int],[Int])]

select' :: LL -> LL
select' = concat . map f
  where
    f (ll,rl) = select ll rl

options :: [Int] -> LL
options = takeWhile (not . null . snd) . concat . iterate select' . select []

options' :: [(Int,Int,Int)]
options' = filter x . concatMap f . options $ digits
  where
    x (a,b,c) = a * b == c
    f (l,r) = map g (options l)
      where
        g (l',r') = (num l', num r', num r)

num :: [Int] -> Int
num = sum . zipWith (*) (iterate (*10) 1) . reverse

--options'' = 

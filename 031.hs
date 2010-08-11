import qualified Data.List  as L
import qualified Data.Array as A

import           Prelude hiding (filter, map)

import           Data.MemoCombinators
import           Data.Function
import           Data.Set
import           Test.QuickCheck ((==>))

main :: IO ()
main = print answer

answer :: Int
answer = size $ g''' 200

divisions :: [Int]
divisions = [1,2,5,10,20,50,100,200]

-- General implementation
type T = Int -> Set [Int]
g :: T -> T
g _ 0 = singleton []
g _ 1 = singleton [1]
g h n = unions . L.map f $ available
  where
    f :: Int -> Set [Int]
    f a = map (L.insertBy compare a) (h (n-a))
    available = L.filter (<= n) divisions

memoFix :: ((a -> b) -> a -> b)-> ((a -> b) -> (a -> b)) -> a -> b
memoFix mem f = let mf = mem (f mf) in mf

g'   = fix g              -- Very slow
g''  = memoFix integral g -- Fast
g''' = memoFix integer  g -- Very fast

integer :: (Int -> b) -> Int -> b
integer f = (A.listArray (0,200) (L.map f [0..200]) A.!) -- Unsafe, but known for these bounds

prop_same_fixed i = (i >= 0) && (i < 20) ==> g' i == g'' i && g' i == g''' i

{--- Specific implementation for reference.

f = integral f'
  where
    f' :: Int -> Integer
    f' 0 = 0
    f' 1 = 1
    f' n = sum [f (n - e) | e <- available]
      where
        available = L.filter (<= n) divisions
-}

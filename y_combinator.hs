-- This file is being used to test some theories regarding monadic fixed-point combinators.
--
--
{-# Language PackageImports #-}

import qualified Data.List  as L
import qualified Data.Array as A

import           Prelude hiding (filter, map)

import           Data.MemoCombinators
import           Data.Function
import           Data.Set
import           Test.QuickCheck ((==>))
import "mtl"     Control.Monad.Trans
import           Control.Monad
import           System.IO

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

-- For reference
fix' :: (a -> a) -> a
fix' f = f (fix f)

mfib :: (Integer -> Integer) -> Integer -> Integer
mfib f 0 = 0
mfib f 1 = 1
mfib f n = f (n-1) + f (n-2)

{-
fix'' :: (a -> IO a) -> IO a
fix'' f = return $ f (print "lol" >> fix'' f)
-}

mfib'' :: Integer -> IO Integer
mfib'' x = do
  return x

mfib' = fixIO mfib''

type MFB a b = (a -> b) -> a -> b
memoFix :: MFB a b -> MFB a b -> a -> b
memoFix mem f = let mf = mem (f mf) in mf

g'   = fix' g             -- Very slow
g''  = memoFix integral g -- Fast
g''' = memoFix integer  g -- Very fast

integer :: (Int -> b) -> Int -> b
integer f = (A.listArray (0,200) (L.map f [0..200]) A.!) -- Unsafe, but known for these bounds

prop_same_fixed i = (i >= 0) && (i < 20) ==> g' i == g'' i && g' i == g''' i

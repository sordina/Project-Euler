-- Confirmed working solution
--
import Data.List (sort, nub)

subRange :: [Int]
subRange = [2..9876] -- TODO: Programatically generate this

digits :: String
digits = ['1'..'9']

newtype Length = Length Int
  deriving Show

len :: Int -> Length
len = Length . (+1) . ceiling . logBase 10 . fromIntegral

unlen :: Length -> Int
unlen (Length n) = 10^(n-1)

y :: Int -> Int
y = floor . (100 * sqrt 10 /) . fromIntegral

co :: Length -> Length
co = len . y . unlen

r :: Int -> (Int,Int)
r n = (unlen l, unlen l * 100 - 1)
  where
    l = co $ len n

range :: Int -> [Int]
range n = let (l,h) = r n in [l..h]

--------

main = print answer

answer = sum . sort . nub $ products

products = map f groups
  where
    f (_,_,n) = n

groups = filter match sets

match (a,b,c) = sort (concatMap show [a,b,c]) == digits

sets = concatMap pairs subRange

pairs :: Int -> [(Int,Int,Int)]
pairs n = map f (range n)
  where
    f x = (n,x,n*x)

import Test.QuickCheck ((==>))

diagSum' :: Integer -> Integer
diagSum' 1 = 1
diagSum' n = ds n + diagSum' (n-1)
  where
    ds 1 = 1
    ds n = sum $ map ($ n) [tr,br,bl,tl]
 
main = print $ diagSum' $ o' 1001

prop_diagSum = and $ zipWith (==) [1,25,101] (map diagSum' [1..])

tr n = (o n)^2
br n = tr (n-1) + (o n) - 1
bl n = tr (n-1) + 2 * ((o n) - 1)
tl n = tr (n-1) + 3 * ((o n) - 1)

o  n = 2 * n - 1
o' n = (n + 1) `div` 2

prop_tr = check tr [1,9,25]
prop_br = check br [1,3,13]
prop_bl = check bl [1,5,17]

check :: (Integer -> Integer) -> [Integer] -> Bool
check fn l = and $ zipWith (==) (map fn [1..]) l

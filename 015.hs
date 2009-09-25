import Binomial

main = print $ paths2 20

main1 = mapM_ (print . pathsSquare) [2..11]
main2 = mapM_ (print . paths2) [2..11]

pathsSquare n = paths n n

paths :: Integer -> Integer -> Integer
paths 1 1 = 2
paths 1 h = (paths 1 (h-1)) + 1
paths w 1 = (paths (w-1) 1) + 1
paths w h = l + r
  where
    l = paths (w-1) h
    r = paths w (h-1)

paths2 w = binomial y x
  where
    y = 2 * w
    x = w

prop_paths2 :: Integer -> Bool
prop_paths2 n = pathsSquare n == paths2 n

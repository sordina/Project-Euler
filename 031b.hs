-- Two solutions taken from http://www.haskell.org/haskellwiki/Euler_problems/31_to_40

coins = [1,2,5,10,20,50,100,200]


-- A (Elegant)

problem_31a = length $ combinations coins !! 200

combinations = foldl (\without p ->
                          let (poor,rich) = splitAt p without
                              with = poor ++ zipWith (++) (map (map (p:)) with)
                                                          rich
                          in with
                     ) ([[]] : repeat [])


-- B (Simple)

problem_31b = length $ withcoins (length coins) 200

withcoins 1 x = [[x]]
withcoins n x = concatMap addCoin [0 .. x `div` coins!!(n-1)]
  where addCoin k = map (++[k]) (withcoins (n-1) (x - k*coins!!(n-1)) )

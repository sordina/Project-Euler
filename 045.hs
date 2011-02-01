main = print . head . filter is_pentagonal $ hexagonal_range

hexagonal_range  = map hexagon [144..]

-- hexagonals are also triagonal
pentagon n = n*(3*n-1) `div` 2
hexagon  n = n*(2*n-1)

is_pentagonal = is_something pentagon

is_something f n = is_s_low 1
  where

    is_s_low b
      | r == n    = True
      | r >  n    = is_s_high (b `div` 2) b
      | otherwise = is_s_low  (b*2)
      where
        r = f b

    is_s_high l h
      | n == r    = True
      | m == l    = False
      | n >= r    = is_s_high m h
      | otherwise = is_s_high l m
      where
        m = (l+h) `div` 2
        r = f m

-- Properties

prop_equal = pentagon 165 == hexagon  143

prop_is_pentagonal = all is_pentagonal [1, 5, 12, 22, 35]

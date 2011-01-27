import qualified Data.List as L
import qualified Data.Char as C

-- One-Indexing
a !!! n = a !! (n - 1)

ints = [1..]
digits = concatMap show ints
powers = take 7 $ iterate (*10) 1
items = map (C.digitToInt . (digits!!!)) powers
answer = L.foldl1' (*) items

prop_item = digits !!! 12 == '1'

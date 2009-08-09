import Control.Arrow
import Data.Array
import Rows
 
input :: Int -> [[Integer]] -> Array (Int,Int) Integer
input n = listArray ((1,1),(n,n)) . concat
 
senses = [(+1) *** id, (+1) *** (+1), id *** (+1), (+1) *** (+ (-1))]
 
prods a = [product xs | i <- range $ bounds a,
                        s <- senses,
                        let is = take 4 $ iterate s i,
                        all (inRange $ bounds a) is,
                        let xs = map (a!) is
          ]
main = (rows "011_matrix.txt") >>= print . maximum . prods . input 20

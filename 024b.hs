import Data.List
 
perms [] = [[]]
perms xs = do
  x <- xs
  -- No need to 'return' as the result is already the same monad 'list'.
  map (x:) (perms $ delete x xs)
        
answer = (perms "0123456789") !! 999999

main = print answer

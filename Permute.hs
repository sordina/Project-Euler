module Permute where

import Data.List (delete)

permute []  = []
permute [a] = [[a]]
permute xs  = do
  x   <- xs
  xs' <- permute (delete x xs)
  return $ x : xs' 

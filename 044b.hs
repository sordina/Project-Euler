import Control.Monad
import Data.Set

main = print problem_44

problem_44 = head solutions

solutions = do
  a <- penta
  b <- takeWhile (<a) penta
  guard $ isPenta (a-b)
  guard $ isPenta (b+a)
  return $ a-b

isPenta = (`member` fromList  penta)
penta = [(n * (3*n-1)) `div` 2 | n <- [1..5000]]

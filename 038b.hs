-- http://www.haskell.org/haskellwiki/Euler_problems/31_to_40#Problem_38
import Data.List

mult n i vs
    | length (concat vs) >= 9 = concat vs
    | otherwise               = mult n (i+1) (vs ++ [show (n * i)])

problem_38 :: Int
problem_38 = maximum . map read . filter ((['1'..'9'] ==) .sort)
               $ [mult n 1 [] | n <- [2..9999]]

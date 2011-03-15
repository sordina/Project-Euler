module Main (main)
where

main :: IO ()
main = mapM_ (print.($100)) [main1]

main1 n = (squareSum $ natural n) - (sumSquares $ natural n)

sumSquares :: [Integer] -> Integer
sumSquares l = sum $ map (^2) l

squareSum :: [Integer] -> Integer
squareSum  l = (sum l) ^ 2

natural n = [1 .. n]

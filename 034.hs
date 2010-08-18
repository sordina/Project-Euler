import Data.List
import Data.Maybe
import Data.Array

main = print answer

answer :: Int
answer = sum . filter curious $ [10 .. digitLimit]

curious :: Int -> Bool
curious x = x == smfd x

smfd :: Int -> Int
smfd = sum . map getFact . digits

digitLimit :: Int
digitLimit = fromMaybe 0 . find f . map (subtract 1) $ (iterate (*10) 10)
  where
    f n = smfd n < n

factorial :: Int -> Int
factorial n = product [1..n]

factorials :: Array Int Int
factorials = listArray (0,9) (map factorial [0..9])

getFact :: Int -> Int
getFact = (factorials !)

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = (n `mod` 10) : digits (n `div` 10)

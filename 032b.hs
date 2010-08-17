import Data.List
import Control.Monad
import Data.Maybe

main = print answers

answer = sum . map (\(_,_,p) -> p) $ answers

answers = concatMap binaryFactors shuffledDigits

binaryFactors :: Int -> [(Int,Int,Int)]
binaryFactors n = catMaybes $ map f [2 .. g n]
  where
    g :: Int -> Int
    g x = fromIntegral . floor $ (sqrt . fromIntegral $ x)
    f x
      | n `mod` x == 0 && dups = Just (x, y, n)
      | otherwise      = Nothing
      where
        y = n `div` x
        dups = all ((1==) . length) . group . sort . concatMap show $ [x,y,n]

shuffledDigits :: [Int]
shuffledDigits = map num shuffledPowerDigits

num :: [Int] -> Int
num = sum . zipWith (*) (iterate (*10) 1) . reverse

shuffledPowerDigits :: [[Int]]
shuffledPowerDigits = concatMap shuffle powerDigits

shuffle :: [Int] -> [[Int]]
shuffle [n] = [[n]]
shuffle l = concat [f e | e <- l]
  where
    f n = map (n:) (shuffle rest)
      where
        rest = delete n l

powerDigits :: [[Int]]
powerDigits = filterM (const [True,False]) digits

digits :: [Int]
digits = [1..9]

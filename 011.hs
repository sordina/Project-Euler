module Main (main)
where

main :: IO ()
main = do
	matrixString <- readMatrixString
	rowMatrix <- return $ parse matrixString
	flat <- return $ flatten rowMatrix
	print $ columnizeNieve rowMatrix

diagonal1 :: [[a]] -> [[a]]
diagonal1 rows@(h:_) = directionalize (rl + 1) $ flatten rows
	where rl = length h

diagonal2 :: [[a]] -> [[a]]
diagonal2 rows@(h:_) = directionalize (rl - 1) $ flatten rows
	where rl = length h


columns :: [[a]] -> [[a]]
columns rows@(h:_) = directionalize rl $ flatten rows
	where rl = length h

directionalize :: Int -> [a] -> [[a]]
directionalize n list = map mapper [0..(n-1)]
	where
		mapper x = takeEvery n $ drop x list

takeEvery :: Int -> [a] -> [a]
takeEvery n [] = []
takeEvery n l = head l : (takeEvery n $ drop n l)

{-
takeEvery :: Int -> [a] -> [[a]]
takeEvery _ [] = []
takeEvery n list = t list []
	where
		back = drop n
		front = take n
		t :: [a] -> [[a]] -> [[a]]
		-- Find a way to do this without reversing
		t [] done = map reverse done
		t xs   [] = t (back xs) $ map (:[]) (front xs)
		t xs done = t (back xs) $ zipWith (:) (front xs) done
-}

prop_takeEvery :: Bool
prop_takeEvery = takeEvery 3 string == done
	where
		string = [1..9]
		done = [1,4,7]

prop_directionalize :: Bool
prop_directionalize = directionalize 3 string == done
	where
		string = [1..9]
		done = [[1,4,7],[2,5,8],[3,6,9]]

{---
 - A function that takes a row based matrix and converts
 - it to a column based matrix.
 - This is sub-optimal as it doesn't work on the tail.
 - It is, however, useful for property verification.
 ---}
columnizeNieve :: [[a]] -> [[a]]
columnizeNieve rowMatrix = cm rowMatrix []
	where
		cm [] complete = complete
		cm todo@(row:rest) done = cm rest $ join row done
			where
				join row [] = map (:[]) row
				join row done = zipWith (\r d -> d ++ [r]) row done

flatten :: [[a]] -> [a]
flatten deep = f deep []
	where
		f :: [[a]] -> [a] -> [a]
		f [] done = done
		f (x:xs) done = f xs $ done ++ x

parse :: String -> [[Integer]]
parse string = map parseLine rowStrings
	where
		rowStrings :: [String]
		rowStrings = lines string

		parseLine :: String -> [Integer]
		parseLine line = map read $ words line

readMatrixString :: IO String
readMatrixString = readFile "011_matrix.txt"

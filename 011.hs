module Main (main)
where

main :: IO ()
main = do
	matrixString <- readMatrixString
	rowMatrix <- return $ parse matrixString
	flat <- return $ flatten rowMatrix
	print $ columnizeNieve rowMatrix

directionalize :: Integer -> [a] -> [[a]]
directionalize period string = d 0 period string
	where
		d :: Integer -> Integer -> [a] -> [[a]]
		d s p str = []

prop_directionalize :: Bool
prop_directionalize = directionalize 3 string == done
	where
		string = [1,4,7,2,5,8,3,6,9]
		done = [[1,2,3],[4,5,6],[7,8,9]]

{---
 - A function that takes a row based matrix and converts
 - it to a column based matrix.
 - This is sub-optimal as it doesn't work on the tail
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

module Main (main)
where

main :: IO ()
main = do
	matrixString <- readMatrixString
	rowMatrix <- return $ parse matrixString
	diagonalMatrix <- return $ roberize rowMatrix
	flat <- return $ flatten rowMatrix
	print $ columnizeNieve rowMatrix

skew :: Integer -> Integer -> [[a]] -> [[a]]
skew offset rowMatrix = cm 0 rowMatrix []
	where
		cm n [] complete = complete
		cm n todo@(row:rest) done = cm n' rest $ join n row done
			where
				n' = n + 1
				join n
					| n < 0 = join' n row done
						where
							join' n row [] = map (:[]) dropped
							join' n row done = zipWith (\r d -> d ++ [r]) dropped done
								where
									dropped = drop (n*offset) row
					| otherwise =

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

module Main (main)
where
import CropEnd

main :: IO ()
main = do
	matrixString <- readMatrixString
	rowMatrix <- return $ parse matrixString
	diagonalMatrix <- return $ rOffset (-1) rowMatrix
	flat <- return $ flatten rowMatrix
	print $ columnizeNieve rowMatrix

rOffset :: Int -> [[a]] -> [[a]]
rOffset _ [] = []
rOffset offset rows = build 0 offset rows
	where
		build :: Int -> Int -> [a] -> [a]
		build upto offset rows = cropped ~~~ build (upto + 1) offset cropped
			where cropped
				| offset < 0 = drop (offset * upto) rows
				| otherwise  = cropEnd (offset * upto) rows

(~~~) :: [a] -> [a] -> [a]
[] ~~~ _ = []
prev ~~~ [] = prev
(hp:prev) ~~~ (hn:next) = hp:[hn:(prev~~~next)]

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

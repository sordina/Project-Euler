module Rows (rows)
where

rows :: FilePath -> IO [[Integer]]
rows path = do
	string <- readMatrixString path
	return $ parse string

parse :: String -> [[Integer]]
parse string = map parseLine rowStrings
	where
		rowStrings :: [String]
		rowStrings = lines string

		parseLine :: String -> [Integer]
		parseLine line = map read $ words line

readMatrixString :: FilePath -> IO String
readMatrixString = readFile

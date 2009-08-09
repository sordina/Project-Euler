module Main-- (main)
where
import Matrix
import qualified Rows as R
import qualified Data.List as DL

main :: IO ()
main = do
	rows     <- R.rows "011_matrix.txt"
	columns  <- return $ DL.transpose rows
	diags    <- return $ diagonals rows
	combined <- return $ rows ++ columns ++ diags
	maximum  <- return $ search combined
	--
	-- And now for something completely different:
	--
	print maximum

search :: [[Integer]] -> Integer
search directions = max' $ map searchDirection directions

searchDirection :: [Integer] -> Integer
searchDirection set = max' (map product $ buildRuns set)

{---
 - This is a shocking implementation of max'
 ---}
max' :: [Integer] -> Integer
max' = foldl max 0

buildRuns :: [Integer] -> [[Integer]]
buildRuns set = extract $ foldl f (1,[],[]) set
	where
		extract (_,_,result) = result
		f (n, [],       done) y = (n+1, [y], [])
		f (n, l@(x:xs), done) y
			| n < 5     = (n+1, l  ++ [y], [])
			| otherwise = (n,   xs ++ [y], l:done)

diagonals :: [[x]] -> [[x]]
diagonals rows
	= concatMap (\f -> f rows) [
			diagonalRight,
			diagonalRight . reverse,
			diagonalRight . DL.transpose,
			diagonalRight . DL.transpose . reverse
		]

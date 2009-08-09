import Data.List
import Rows

main :: IO ()
main = do
	rows "011_matrix.txt" >>= print . res

rowToMax l = maximum (map (product . take 4) (tails l)) 
 
diag  = zipWith rotate [0..]
    where rotate n l = (drop n l) ++ (take n l)
 
rdiag = diag . map reverse

-- find the result
res matrix = maximum (concatMap ($ matrix) [rowValues, colValues, diagValues, rDiagValues])
    where rowValues = map rowToMax
          colValues = rowValues . transpose
          diagValues = colValues . diag
          rDiagValues = colValues . rdiag

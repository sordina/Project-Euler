import List
import Rows
 
main = do
	grid <- rows "011_matrix.txt"
	print $ maximum $ map (flip ($ 4) grid) [maxInRows, maxInCols, maxInLR, maxInRL]

myMaximum [] = 0
myMaximum xs = maximum xs
 
takeBy n = filter ((n==) . length) . map (take n) . tails
 
maxBy n = myMaximum . map product . takeBy n
 
maxInRows n = maximum . map (maxBy n)
 
maxInCols n = maxInRows n . transpose
 
maxInLR n = maxInRows n . transpose . zipWith drop [0..]
 
maxInRL n = maxInLR n . map reverse

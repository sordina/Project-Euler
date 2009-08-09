import Rows
import List

lineProducts x
  | length x < 4 = []
  | otherwise = product (take 4 x) : lineProducts (tail x)                                                            
 
allOrient g = g ++ transpose g ++ diags g ++ (diags . reverse) g                                                       
    where diags gr = topright gr ++ (topright . transpose) gr
          topright = (transpose . slash)
          slash gr = zipWith drop [0..] gr
 
main = do
	set <- rows "011_matrix.txt"
	print $ maximum $ foldr1 (++) $ map lineProducts $ allOrient set

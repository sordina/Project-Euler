import Data.Set (fromList)
import Data.List

main = print answer

answer = find satisfactory [1..]

satisfactory n = (==1) . length . group . map (fromList . show . (*n)) $ multiples

multiples = [2..6]

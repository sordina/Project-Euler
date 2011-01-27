import Data.Char
import Data.Maybe
import Text.Regex.Posix

main = print . countTriangleWords =<< readWords

countTriangleWords :: Integral n => [String] -> n
countTriangleWords = fromIntegral . length . filter isTriangleWord

readWords :: IO [String]
readWords =  fmap (map head . fromMaybe [] . (=~~ "[^,\"]+")) $ readFile "042_words.txt"

isTriangleWord :: String -> Bool
isTriangleWord = isTriangle . wordVal

-- TODO: Use quadratic equation for constant-time position estimation
isTriangle :: Integral n => n -> Bool
isTriangle n = (n' ==) . last . takeWhile (<= n') $ triangles
  where n' = fromIntegral n

-- triangles = 1 : zipWith (+) triangles [2..]
triangles = map triangle [1..]

triangle :: Integral n => n -> n
triangle n = n * (n+1) `div` 2

wordVal :: Integral n => String -> n
wordVal = sum . map letterVal

letterVal :: Integral n => Char -> n
letterVal = (+1) . subtract aVal . fromIntegral . ord

aVal :: Integral n => n
aVal = fromIntegral $ ord 'A'

-- Properties

prop_wordVal = wordVal "SKY" == triangle 10

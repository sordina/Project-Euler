import Data.List (init, sort, foldl1')
import Data.Char (ord)
import Text.ParserCombinators.Parsec

-- Guts of the problem...

solve :: [String] -> Integer
solve = sum . zipWith f [1..] . sort
  where
    f :: Integer -> String -> Integer
    f pos name = pos * value name


value :: String -> Integer
value = fromIntegral . sum . map ((+1) . subtract a . ord)
  where
    a :: Int
    a = ord 'A'

prop_value_1 = 53 == value "COLIN"
prop_value_2 = 1 == value "A"

-- Plumbing follows...

parseCSV :: Parser [String]
parseCSV = sepBy parseQuoted (oneOf ",")

parseQuoted :: Parser String
parseQuoted = do
  char '"'
  value <- many $ noneOf ['"']
  char '"'
  return $ value

main :: IO ()
main = readFile "022_names.txt"
  >>= return . parse parseCSV "Names"
  >>= return . splitError
  >>= return . solve
  >>= print
  where
    splitError :: Either ParseError [String] -> [String]
    splitError (Left _) = error "Damn. Couldn't parse shit."
    splitError (Right val) = val

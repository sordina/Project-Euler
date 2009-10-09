----------------
{- Problem 17 -}
----------------


-- Main Program
--
main = print $ solve [1..1000]


-- Properties
--
prop_solve = solve [1..5] == 19

prop_english = (l 342 == 23) && (l 115 == 20)
  where l = length . english


-- Library Types
--
one_9, one_19, twenty_99, one_99, one_999 :: [String]


-- Library Definitions
--
solve range = sum lengths
  where
    lengths = map length numbers
    numbers = [english n | n <- range]

english n = one_1000 !! (n-1)

one_19 = [
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
  "ten",
  "eleven",
  "twelve",
  "thirteen",
  "fourteen",
  "fifteen",
  "sixteen",
  "seventeen",
  "eighteen",
  "nineteen"
  ]

one_9 = take 9 one_19

one_99 = one_19 ++ twenty_99

tens = [
  "twenty",
  "thirty",
  "forty",
  "fifty",
  "sixty",
  "seventy",
  "eighty",
  "ninety"
  ]

twenty_99 = concatMap f tens
  where
    f n = n : map (n ++) one_9

hundreds = map (++ "hundred") one_9

one_999 = one_99 ++ concatMap f hundreds
  where
    f n = n : map ((n ++ "and") ++) one_99

one_1000 = one_999 ++ ["one" ++ "thousand"]

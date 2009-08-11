main = readFile "013_numbers.txt" >>= putStrLn . take 10 . show . sum . map read . words 

import Data.List
import Data.Maybe

main :: IO ()
main = print $ length fl
  where
    l  = [0 :: Integer .. 10000-1]
    fl = filter lychrel l

palindromic :: (Show n, Integral n) => n -> Bool
palindromic n = s == reverse s where s = show n

flipadd :: (Show n, Read n, Integral n) => n -> n
flipadd n = n + read (reverse (show n))

iterpal :: (Show n, Read n, Integral n) => n -> [n]
iterpal = iterate flipadd

lychrel :: (Show n, Read n, Integral n) => n -> Bool
lychrel = fromMaybe True . fmap not . find id . take 49 . drop 1 . map (palindromic) . iterpal

tests :: IO ()
tests = mapM_ print [ flipadd 47 == (121 :: Int)
                    , palindromic (121 :: Int)
                    , palindromic $ flipadd (47  :: Int)
                    , (== 3) $ length $ takeWhile (not . palindromic) $ iterpal (349 :: Int)
                    , lychrel (10677 :: Integer)
                    , not $ lychrel (349 :: Int)
                    , lychrel (4994 :: Integer)
                    ]

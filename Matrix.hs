module Matrix
where

type Matrix x = [Row x]
type Row x = [x]

diagonalRight :: Matrix t -> [[t]]
diagonalRight m@(x:xs) = foldr zipD emptys $ dropD m
	where
		emptys = map (\_ -> []) x

dropD :: Matrix t -> [[t]]
dropD = d 0
	where
		d n [] = []
		d n (x:xs) = (drop n x) : (d n' xs)
			where
				n' = n + 1

zipD :: [t] -> [[t]] -> [[t]]
zipD = zipWithKeep f
	where
		f :: Maybe t -> Maybe [t] -> [t]
		f  Nothing (Just ys) = ys
		f (Just x)  Nothing  = [x]
		f (Just x) (Just ys) = x : ys

zipWithKeep :: (Maybe x -> Maybe y -> z) -> [x] -> [y] -> [z]
zipWithKeep _ []     []     = []
zipWithKeep f []     (y:ys) = (f  Nothing (Just y)) : (zipWithKeep f [] ys)
zipWithKeep f (x:xs) []     = (f (Just x)  Nothing) : (zipWithKeep f xs [])
zipWithKeep f (x:xs) (y:ys) = (f (Just x) (Just y)) : (zipWithKeep f xs ys)

prop_zipWithKeep =
	zipWithKeep f [1,2,3] [1,2] == [2,4,3]
		where
			f  Nothing (Just y) = y
			f (Just x)  Nothing = x
			f (Just x) (Just y) = x + y

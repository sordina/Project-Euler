module CropEnd
where

cropEnd :: Int -> [a] -> [a]
cropEnd 0 list = list
cropEnd _ [] = []
cropEnd n xs = take headlen xs
	where headlen = length xs - n

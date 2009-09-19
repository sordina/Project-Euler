{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List (foldl1')
import Data.IORef
import Control.Concurrent
import Control.Monad

secondInMicros = 10^6
sequenceEnd = 10^6

main = do
	upto <- newIORef (0 :: Int)
	done <- newIORef False

	go upto done

go upto done = 
	forkIO status >> calculate
	where
		status = doit
			where
				doit = do
					threadDelay secondInMicros
					print =<< readIORef upto
					isDone <- readIORef done
					if isDone
						then
							return ()
						else
							doit

		calculate = do
			print =<< calc upto [1 .. sequenceEnd]
			writeIORef done True

calc :: IORef Int -> [Int] -> IO Int
calc io range = do
	bla <- mapM promote range
	return $ myMaximum bla
	where
		promote :: Int -> IO Int
		promote x = do
			writeIORef io x
			return $ numeric x

myMaximum :: [Int] -> Int
myMaximum = foldl1' max

-- Numeric version of f
--
numeric :: Int -> Int
numeric = f 1
	where
		f :: Int -> Int -> Int
		f ! acc 1 = acc
		f ! acc n
			| even n    = f acc' $ n `div `2
			| otherwise = f acc' $ 3 * n + 1
			where
				acc' = acc + 1

prop_numeric :: Int -> Bool
prop_numeric n
	| n < 1 = True
	| n > 100000 = True
	| otherwise = (numeric n) == (length $ naieve n)

-- Naieve version of f
--
naieve = f
	where
		f :: Int -> [Integer]
		f 1 = [1]
		f n
			| even n    = n' : (f $ n `div `2)
			| otherwise = n' : (f $ 3*n + 1)
			where
				n' = fromIntegral n

module Memoization (memoizeFixInt, memoizeInt) where

memoizeFixInt :: ((Integer -> a) -> (Integer -> a)) -> Integer -> a
memoizeFixInt f =
   let mf = memoize (f mf) in mf

memoize :: (Integer -> a) -> (Integer -> a)
memoize = memoizeInt

memoizeInt :: (Integer -> a) -> (Integer -> a)
memoizeInt f = (fmap f (naturals 1 0) !!!)

data NaturalTree a = Node a (NaturalTree a) (NaturalTree a)

Node a _  _  !!! 0 = a 
Node _ tl tr !!! n =
   if odd n
     then tl !!! top
     else tr !!! (top-1)
        where top = n `div` 2

instance Show t => Show (NaturalTree t) where
	show (Node a b c) = "[" ++ show a ++ "," ++ show b ++ show c

instance Functor NaturalTree where
	fmap f (Node a tl tr) = Node (f a) (fmap f tl) (fmap f tr)

naturals r n =
   Node n
     ((naturals $! r2) $! (n+r))
     ((naturals $! r2) $! (n+r2))
        where r2 = 2*r

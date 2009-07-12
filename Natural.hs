module Natural (Natural, toNatural, fromNatural)
where

newtype Natural = MakeNatural Integer

toNatural               :: Integer -> Natural
toNatural x | x < 0     = error "Can't create negative naturals!" 
            | otherwise = MakeNatural x

fromNatural             :: Natural -> Integer
fromNatural (MakeNatural i) = i

instance Num Natural where
    fromInteger         = toNatural
    signum _            = 1
    abs x               = x
    x + y               = toNatural (fromNatural x + fromNatural y)
    x - y               = let r = fromNatural x - fromNatural y in
                            if r < 0 then error "Unnatural subtraction"
                                     else toNatural r
    x * y               = toNatural (fromNatural x * fromNatural y)

instance Show Natural where
    show                = show . fromNatural

instance Eq Natural where
    a == b              = (fromNatural a) == (fromNatural b)

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Function

getRows :: IO [String]
getRows = lines <$> readFile "Problems/poker.txt"

main :: IO ()
main = getRows >>= print . length . filter winner

winner :: String -> Bool
winner =   words
       >>> (hand . take 5 &&& hand . drop 5)
       >>> uncurry (>)

hand :: [String] -> Hand
hand = (classify &&& id) . map readCard

type Hand = (Classification, [Card])

readRank :: String -> Rank
readRank = Rank . p
  where
    p :: String -> Int
    p "T" = 10
    p "J" = 11
    p "Q" = 12
    p "K" = 13
    p "A" = 14
    p x   = read x

readCard :: String -> Card
readCard [r,s] = Card (readRank [r]) (read [s])
readCard c     = error $ "Invalid Card " ++ c

classify :: [Card] -> Classification
classify h | e (sameSuit >> has 14 >> run) = RoyalFlush
           | e (sameSuit >> run)           = StraightFlush
           | e (kind 4)                    = FourOfAKind
           | e (kind 3 >> kind 2)          = FullHouse
           | e  sameSuit                   = Flush
           | e  run                        = Straight
           | e (kind 3)                    = ThreeOfAKind
           | e (kind 2 >> kind 2)          = TwoPairs
           | e (kind 2)                    = OnePair
           | otherwise                     = HighCard
  where
    e = execConstraints h

execConstraints :: [Card] -> State ([Card], Bool) () -> Bool
execConstraints h s = snd $ execState s (h, True)

next :: ([Card] -> ([Card],Bool)) -> State ([Card],Bool) ()
next f = do (s,b) <- get
            when b $ put $ f s

sameSuit :: State ([Card],Bool) ()
sameSuit = next $ id &&& samaSama . map suit

has :: Int -> State ([Card],Bool) ()
has r = next $ id &&& any (== r) . map (unRank . rank)

run :: State ([Card],Bool) ()
run = next $ id &&& (map (unRank . rank) >>> sort >>> zipWith (+) [1,0..] >>> samaSama)

kind :: Int -> State ([Card],Bool) ()
kind n = next $ concat . filter ((/= n) . length) . groupBy ((==) `on` rank)
              &&&        any    ((== n) . length) . groupBy ((==) `on` rank)

samaSama :: Eq x => [x] -> Bool
samaSama l = and $ zipWith (==) l (tail l)

-- Data

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq, Ord)
data Suit = S | C | D | H                       deriving (Show, Eq, Ord, Read)
data Rank = Rank { unRank :: Int }              deriving (Show, Eq, Ord)

data Classification =   HighCard       --  Highest value card.
                      | OnePair        --  Two cards of the same value.
                      | TwoPairs       --  Two different pairs.
                      | ThreeOfAKind   --  Three cards of the same value.
                      | Straight       --  All cards are consecutive values.
                      | Flush          --  All cards of the same suit.
                      | FullHouse      --  Three of a kind and a pair.
                      | FourOfAKind    --  Four cards of the same value.
                      | StraightFlush  --  All cards are consecutive values of same suit.
                      | RoyalFlush     --  Ten, Jack, Queen, King, Ace, in same suit.
                      deriving (Eq, Ord, Show)

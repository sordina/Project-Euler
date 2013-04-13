import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Maybe
import Control.Monad.Identity
import Data.List
import Data.Maybe
import Data.Ord
import Data.Function
import Text.Groom

-- | TODO: Use read instances instead for better succinctness -
-- | http://stackoverflow.com/questions/5520940/creating-instance-of-read-type-class-in-haskell-for-custom-data-type

-- Data

type Hand           = [Card]
type CardA a        = StateT [Card] (MaybeT Identity) a
type CardU          = CardA  ()
type CardC          = CardA  Card
type CardF          = CardA  Classification

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq, Ord)
data Suit = S | C | D | H                       deriving (Show, Eq, Ord, Read)
data Rank = Rank { unRank :: Int }              deriving (Show, Eq, Ord)

data Classification = HighCard      Rank       --  Highest value card.
                    | OnePair       Rank       --  Two cards of the same value.
                    | TwoPairs      Rank Rank  --  Two different pairs.
                    | ThreeOfAKind  Rank       --  Three cards of the same value.
                    | Straight      Rank       --  All cards are consecutive values.
                    | Flush         Rank       --  All cards of the same suit.
                    | FullHouse     Rank Rank  --  Three of a kind and a pair.
                    | FourOfAKind   Rank       --  Four cards of the same value.
                    | StraightFlush Rank       --  All cards are consecutive values of same suit.
                    | RoyalFlush               --  Ten, Jack, Queen, King, Ace, in same suit.
                    deriving (Eq, Ord, Show)

-- Functions

main :: IO ()
main = getRows >>= print . length . filter winner

getRows :: IO [String]
getRows = lines <$> readFile "Problems/poker.txt"

winner :: String -> Bool
winner =   words
       >>> (hand . take 5 &&& hand . drop 5)
       >>> uncurry (>)

hand :: [String] -> (Classification, [Int])
hand = (classify &&& reverse . sort . map getRank) . map readCard

readCard :: String -> Card
readCard [r,s] = Card (readRank [r]) (read [s])
readCard c     = error $ "Invalid Card " ++ c

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

classify :: Hand -> Classification
classify hnd = fromMaybe (highest hnd) (join $ find isJust evaluated)
   where
    evaluated  = map (runIdentity . runMaybeT . flip evalStateT hnd) classifications

classifications :: [ CardF ]
classifications = [ do sameSuit
                       has 14
                       _ <- run
                       return RoyalFlush

                  , do sameSuit
                       top  <- run
                       return $ StraightFlush $ rank top

                  , do top <- kind 4
                       return $ FourOfAKind $ rank top

                  , do top1 <- kind 3
                       top2 <- kind 2
                       return $ FullHouse (rank top1) (rank top2)

                  , do sameSuit
                       top <- best
                       return $ Flush $ rank top

                  , do top <- run
                       return $ Straight $ rank top

                  , do top <- kind 3
                       return $ ThreeOfAKind $ rank top

                  , do top1 <- kind 2
                       top2 <- kind 2
                       return $ TwoPairs (rank top1) (rank top2)

                  , do top1 <- kind 2
                       return $ OnePair $ rank top1
                  ]

highest :: Hand -> Classification
highest = HighCard . rank . maximum

best :: CardC
best = maybeFail . listToMaybe . reverse . sort =<< get

sameSuit :: CardU
sameSuit = get >>= samaSama . map suit

has :: Int -> CardU
has r = get >>= guard . isJust . find ((==r) . getRank)

getRank :: Card -> Int
getRank = unRank . rank

run :: CardC
run = do cards <- gets $ map getRank >>> sort >>> zipWith (+) [1,0..]
         samaSama cards
         best

kind :: Int -> CardC
kind n = do cards    <- gets $ groupBy ((==) `on` rank) . sortBy (comparing rank)
            matched  <- maybeFail $ find ((==n) . length) cards
            put       $ concat . filter ((/= n) . length) $ cards
            maybeFail $ listToMaybe $ matched

samaSama :: Eq x => [x] -> CardU
samaSama = guard . allSame

allSame :: Eq x => [x] -> Bool
allSame l = and $ zipWith (==) l (tail l)

maybeFail :: Maybe a -> CardA a
maybeFail (Just a) = return a
maybeFail Nothing  = mzero

-- Tests

tests :: IO ()
tests = mapM_ (uncurry test) [(False, "5H 5C 6S 7S KD 2C 3S 8S 8D TD")
                             ,(True,  "5D 8C 9S JS AC 2C 5C 7D 8S QH")
                             ,(False, "2D 9C AS AH AC 3D 6D 7D TD QD")
                             ,(True,  "4D 6S 9H QH QC 3D 6D 7H QD QS")
                             ,(True,  "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")]

  where

    test :: Bool -> String -> IO ()
    test b s = if b == winner s then print $ "Test Passed"
                                else do
                                  print $ "Test Failed: " ++ s
                                  putStrLn $ groom $ hands s

    hands = words >>> (hand . take 5 &&& hand . drop 5)

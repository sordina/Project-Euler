import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Function
import Data.Ord
import Text.Groom

getRows :: IO [String]
getRows = lines <$> readFile "Problems/poker.txt"

main :: IO ()
main = getRows >>= print . length . filter winner

winner :: String -> Bool
winner =   words
       >>> (hand . take 5 &&& hand . drop 5)
       >>> uncurry (>)

hand :: [String] -> ClassifiedHand
hand = (classify &&& reverse . sort) . map readCard

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

classifications :: [StateT (MaybeT ClassifiedHand) ]
classifications [ do suit <- sameSuit
                     card <- has 14
                     top  <- run
                     return RoyalFlush

                , do suit <- sameSuit
                     top  <- run
                     return $ StraightFlush $ rank top

                , do top <- kind 4
                     return $ FourOfAKind $ rank top

                , do top1 <- kind 3
                     top2 <- kind 2
                     return $ FullHouse (rank top1) (rank top2)

                , do top <- sameSuit
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

highest = undefined

classify :: Hand -> (Classification, Hand)
classify h = fromMaybe (highest h) (find isJust ran)
  where
    ran = map (\x -> runState x h) classifications

execConstraints :: Hand -> HandStateM -> Bool
execConstraints h s = snd $ runState s (h, True)

next :: (Hand -> HandState) -> HandStateM
next f = do (s,b) <- get
            when b $ put $ f s

sameSuit :: HandStateM
sameSuit = next $ id &&& samaSama . map suit

has :: Int -> HandStateM
has r = next $ id &&& any (== r) . map (unRank . rank)

run :: HandStateM
run = next $ id &&& (map (unRank . rank) >>> sort >>> zipWith (+) [1,0..] >>> samaSama)

kind :: Int -> HandStateM
kind n = next $ concat . filter ((/= n) . length) . grouper
              &&&        any    ((== n) . length) . grouper
  where
    grouper = groupBy ((==) `on` rank) . sortBy (comparing rank)

samaSama :: Eq x => [x] -> Bool
samaSama l = and $ zipWith (==) l (tail l)

-- Data

type Hand           = [Card]
type HandState      = (Hand, Bool)
type HandStateM     = State HandState Classification
type ClassifiedHand = (Classification, Hand)

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

-- Tests

test :: Bool -> String -> IO ()
test b s = if b == winner s then print $ "Test Passed"
                            else do
                              print $ "Test Failed: " ++ s
                              putStrLn $ groom $ hands s

tests :: IO ()
tests = mapM_ (uncurry test) [(False, "5H 5C 6S 7S KD 2C 3S 8S 8D TD")
                             ,(True,  "5D 8C 9S JS AC 2C 5C 7D 8S QH")
                             ,(False, "2D 9C AS AH AC 3D 6D 7D TD QD")
                             ,(True,  "4D 6S 9H QH QC 3D 6D 7H QD QS")
                             ,(True,  "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")]

hands :: String -> (ClassifiedHand, ClassifiedHand)
hands = words >>> (hand . take 5 &&& hand . drop 5)

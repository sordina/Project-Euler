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

-- Data

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

-- Types

type Hand    = [Card]
type CardA a = StateT [Card] (MaybeT Identity) a
type CardU   = CardA  ()
type CardC   = CardA  Card

-- Instances

instance Read Card where
  readsPrec _ [r,s] = [ (Card (read [r]) (read [s]), "" ) ]
  readsPrec _ _     = [ ]

instance Read Rank where
  readsPrec _ l@[_] = [ (Rank (p l), "") ] where p :: String -> Int
                                                 p "T" = 10
                                                 p "J" = 11
                                                 p "Q" = 12
                                                 p "K" = 13
                                                 p "A" = 14
                                                 p "1" = 14
                                                 p x   = read x
  readsPrec _ _ = [ ]

-- Main Program

main :: IO ()
main = getRows >>= print . length . filter winner

getRows :: IO [String]
getRows = lines <$> readFile "Problems/poker.txt"

winner :: String -> Bool
winner =   words
       >>> (hand . take 5 &&& hand . drop 5)
       >>> uncurry (>)

-- We don't actually need the hand for the Euler problem, but it does help for edge-cases
hand :: [String] -> (Classification, Hand)
hand = (classify &&& reverse . sort) . map read

-- The rules of poker hand classification!

classify :: Hand -> Classification
classify hnd = fromMaybe (highest hnd) (join $ find isJust evaluated)
   where
    evaluated  = map (runIdentity . runMaybeT . flip evalStateT hnd) classifications

classifications :: [ CardA Classification ]
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

-- Helpers

highest :: Hand -> Classification
highest = HighCard . rank . maximum

best :: CardC
best = maybeFail . listToMaybe . reverse . sort =<< get

sameSuit :: CardU
sameSuit = get >>= samaSama . map suit

has :: Int -> CardU
has r = get >>= guard . isJust . find ((==r) . getRankInt)

getRankInt :: Card -> Int
getRankInt = unRank . rank

run :: CardC
run = do cards <- gets $ map getRankInt >>> sort >>> zipWith (+) [1,0..]
         samaSama cards
         best

kind :: Int -> CardC
kind n = do cards    <- gets $ groupBy ((==) `on` rank) . sortBy (comparing rank)
            matched  <- maybeFail $ find ((==n) . length) cards
            put       $ concat . deleteWhen ((==n) . length) $ cards
            maybeFail $ listToMaybe $ matched

samaSama :: Eq x => [x] -> CardU
samaSama = guard . allSame

allSame :: Eq x => [x] -> Bool
allSame l = and $ zipWith (==) l (tail l)

maybeFail :: Maybe a -> CardA a
maybeFail (Just a) = return a
maybeFail Nothing  = mzero

-- Why doesn't this exist already?
deleteWhen :: (a -> Bool) -> [a] -> [a]
deleteWhen f = uncurry (++) . (id *** drop 1) . break f

-- Tests

tests :: IO ()
tests = mapM_ (uncurry test) [(False, "5H 5C 6S 7S KD 2C 3S 8S 8D TD")
                             ,(True , "5D 8C 9S JS AC 2C 5C 7D 8S QH")
                             ,(False, "2D 9C AS AH AC 3D 6D 7D TD QD")
                             ,(True , "4D 6S 9H QH QC 3D 6D 7H QD QS")
                             ,(True , "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")
                             ,(False, "JS QH 9C AS 5C QS JC 3D QC 7C")  -- JS QH 9C AS 5C high A  <  QS JC 3D QC 7C pair Q
                             ,(True , "JC 9C KH JH QS QC 2C TS 3D AD")  -- JC 9C KH JH QS pair J  >  QC 2C TS 3D AD high A
                             ,(True , "5D JH AC 5C 9S TS 4C JD 8C KS")  -- 5D JH AC 5C 9S pair 5  >  TS 4C JD 8C KS high K
                             ,(False, "KC AS 2D KH 9H 2C 5S 4D 3D 6H")  -- KC AS 2D KH 9H pair K  <  2C 5S 4D 3D 6H straight
                             ,(False, "TH AH 2D 8S JC 3D 8C QH 7S 3S")  -- TH AH 2D 8S JC high A  <  3D 8C QH 7S 3S pair 3
                             ,(False, "1H 2H 3D 4S 5C 2D 3C 4H 5S 6S")  -- Straight 5             <  Straight 6
                             ,(False, "AH 2D 3S 4C 5C 3D 3C 4H 5S 6S")  -- High A                 <  Pair
                             ,(True , "2H 3D 4S 5C 6C 3D 3C 4H 5S 6S")  -- Straight               >  Pair
                             ,(True , "2H 3D 4S 5C 8C 2D 3C 4H 5S 7S")  -- High 8                 >  High 7
                             ,(True , "1C 2C 3C 4C 5C 2D 3C 4H 5S 6S")  -- StraightFlush          >  Straight
                             ,(False, "KH JS 4H 5D 9D TC TD QC JD TS")  -- High K                 <  Three T
                             ,(True , "QS QD AC AD 4C 6S 2D AS 3H KC")
                             ,(False, "4C 7C 3C TD QS 9C KC AS 8D AD")  -- High Q                 <  Pair A
                             ,(False, "KC 7H QC 6D 8H 6S 5S AH 7S 8C")  -- High K                 <  High A
                             ,(False, "3S AD 9H JC 6D JD AS KH 6S JH")  -- High A                 <  Pair J
                             ,(False, "AD 3D TS KS 7H JH 2D JS QD AC")  -- High A                 <  Pair J
                             ,(False, "9C JD 7C 6D TC 6H 6C JC 3D 3S")  -- High J                 <  Pair 3
                             ,(True , "QC KC 3S JC KD 2C 8D AH QS TS")
                             ,(True , "AS KD 3D JD 8H 7C 8C 5C QD 6C")
                             ,(False, "8H QD 4H JC AS KH KS 3C 9S 6D")] -- 8H QD 4H JC AS high A  <  KH KS 3C 9S 6D pair K


  where

    test :: Bool -> String -> IO ()
    test b s = if b == winner s then    print    $ "Test Passed"
                                else do print    $ "Test Failed: " ++ s
                                        putStrLn $ groom $ hands s

    hands = words >>> (hand . take 5 &&& hand . drop 5)

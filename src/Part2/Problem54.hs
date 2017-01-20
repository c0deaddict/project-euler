module Part2.Problem54 where

import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (group, sort, sortBy)
import           Data.Maybe                 (catMaybes)
import           Data.Ord                   (comparing)
import           Network.HTTP.Conduit

--
-- Problem 54: Poker hands
--
-- In the card game poker, a hand consists of five cards and are ranked, from
-- lowest to highest, in the following way:
--
--   High Card:       Highest value card.
--   One Pair:        Two cards of the same value.
--   Two Pairs:       Two different pairs.
--   Three of a Kind: Three cards of the same value.
--   Straight:        All cards are consecutive values.
--   Flush:           All cards of the same suit.
--   Full House:      Three of a kind and a pair.
--   Four of a Kind:  Four cards of the same value.
--   Straight Flush:  All cards are consecutive values of same suit.
--   Royal Flush:     Ten, Jack, Queen, King, Ace, in same suit.
--
-- The cards are valued in the order:
--
--   2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
--
-- If two players have the same ranked hands then the rank made up of the
-- highest value wins; for example, a pair of eights beats a pair of fives
-- (see example 1 below). But if two ranks tie, for example, both players have
-- a pair of queens, then highest cards in each hand are compared (see
-- example 4 below); if the highest cards tie then the next highest cards are
-- compared, and so on.
--
-- Consider the following five hands dealt to two players:
--
-- Hand	 	Player 1             Player 2	 	              Winner
-- 1	 	  5H 5C 6S 7S KD       2C 3S 8S 8D TD
--        Pair of Fives        Pair of Eights           Player 2
-- 2	 	  5D 8C 9S JS AC       2C 5C 7D 8S QH
--        Highest card Ace     Highest card Queen       Player 1
-- 3	 	  2D 9C AS AH AC       3D 6D 7D TD QD
--        Three Aces           Flush with Diamonds      Player 2
-- 4	 	  4D 6S 9H QH QC       3D 6D 7H QD QS
--        Pair of Queens       Pair of Queens
--        Highest card Nine    Highest card Seven       Player 1
-- 5	 	  2H 2D 4C 4D 4S       3C 3D 3S 9S 9D
--        Full House           Full House
--        With Three Fours     with Three Threes        Player 1
--
-- The file, poker.txt, contains one-thousand random hands dealt to two players.
-- Each line of the file contains ten cards (separated by a single space): the
-- first five are Player 1's cards and the last five are Player 2's cards.
-- You can assume that all hands are valid (no invalid characters or repeated
-- cards), each player's hand is in no specific order, and in each hand there
-- is a clear winner.
--
-- How many hands does Player 1 win?

data Suit = Hearts
          | Diamonds
          | Clubs
          | Spades deriving (Eq)

data Number = Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine
            | Ten
            | Jack
            | Queen
            | King
            | Ace deriving (Ord, Eq)

data Card = Card Suit Number deriving (Eq)

data Hand = Hand [Card] deriving (Eq)

data Score = HighCard [Number]
           | OnePair Number Number Number Number
           | TwoPair Number Number Number
           | ThreeOfAKind Number Number Number
           | Straight Number
           | Flush [Number]
           | FullHouse Number Number
           | FourOfAKind Number Number
           | StraightFlush Number
           deriving (Show, Eq, Ord)

instance Show Suit where
  show Hearts   = "H"
  show Diamonds = "D"
  show Clubs    = "C"
  show Spades   = "S"

instance Show Number where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "T"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

instance Show Card where
  show (Card suit number) = show number ++ show suit

instance Show Hand where
  show (Hand cards) = unwords $ map show cards

instance Ord Hand where
  compare a b = case comparing score a b of
    EQ    -> comparing handDesc a b
    other -> other

suit :: Card -> Suit
suit (Card s _) = s

number :: Card -> Number
number (Card _ n) = n

handDesc :: Hand -> [Number]
handDesc (Hand cards) = numbersDesc cards

numbersDesc :: [Card] -> [Number]
numbersDesc = sortBy (flip compare) . fmap number

isStraight :: [Card] -> Bool
isStraight = consecutive . numbersDesc where
  consecutive [Six,Five,Four,Three,Two]   = True
  consecutive [Seven,Six,Five,Four,Three] = True
  consecutive [Eight,Seven,Six,Five,Four] = True
  consecutive [Nine,Eight,Seven,Six,Five] = True
  consecutive [Ten,Nine,Eight,Seven,Six]  = True
  consecutive [Jack,Ten,Nine,Eight,Seven] = True
  consecutive [Queen,Jack,Ten,Nine,Eight] = True
  consecutive [King,Queen,Jack,Ten,Nine]  = True
  consecutive [Ace,King,Queen,Jack,Ten]   = True
  consecutive _                           = False

isFlush :: [Card] -> Bool
isFlush cards = all ((==) firstSuit . suit) cards where
  firstSuit = suit $ head cards

-- Run length encoded, longest sequence first, then ordered on number
groupedNumbers :: [Card] -> [(Int, Number)]
groupedNumbers cards = sortBy (flip lengthAndNumber) grouped where
  grouped = rle <$> group (sort $ number <$> cards)
  rle xs = (length xs, head xs)
  lengthAndNumber a b =
    case comparing fst a b of
      EQ    -> comparing snd a b
      other -> other

score :: Hand -> Score
score (Hand cards) =
  case groupedNumbers cards of
    xs | isFlush cards && isStraight cards ->
      StraightFlush $ snd $ head xs
    [(4,x), (1,y)] -> FourOfAKind x y
    [(3,x), (2,y)] -> FullHouse x y
    _ | isFlush cards -> Flush $ numbersDesc cards
    xs | isStraight cards -> Straight $ snd $ head xs
    [(3,x), (1,y), (1,z)] -> ThreeOfAKind x y z
    [(2,x), (2,y), (1,z)] -> TwoPair x y z
    [(2,x), (1,a), (1,b), (1,c)] -> OnePair x a b c
    _ -> HighCard $ numbersDesc cards

readSuit :: Char -> Maybe Suit
readSuit 'H' = Just Hearts
readSuit 'D' = Just Diamonds
readSuit 'C' = Just Clubs
readSuit 'S' = Just Spades
readSuit _   = Nothing

readNumber :: Char -> Maybe Number
readNumber '2' = Just Two
readNumber '3' = Just Three
readNumber '4' = Just Four
readNumber '5' = Just Five
readNumber '6' = Just Six
readNumber '7' = Just Seven
readNumber '8' = Just Eight
readNumber '9' = Just Nine
readNumber 'T' = Just Ten
readNumber 'J' = Just Jack
readNumber 'Q' = Just Queen
readNumber 'K' = Just King
readNumber 'A' = Just Ace
readNumber _   = Nothing

readCard :: String -> Maybe (Card, String)
readCard (numberCh:suitCh:rest) = do
  number <- readNumber numberCh
  suit <- readSuit suitCh
  return (Card suit number, rest)
readCard _ = Nothing

skipSpaces :: String -> String
skipSpaces []        = []
skipSpaces (' ':str) = skipSpaces str
skipSpaces str       = str

readHand :: String -> Maybe (Hand, String)
readHand str = do
  (c1, rest1) <- readCard str
  (c2, rest2) <- readCard (skipSpaces rest1)
  (c3, rest3) <- readCard (skipSpaces rest2)
  (c4, rest4) <- readCard (skipSpaces rest3)
  (c5, rest5) <- readCard (skipSpaces rest4)
  return (Hand [c1, c2, c3, c4, c5], rest5)

readTwoHands :: String -> Maybe (Hand, Hand)
readTwoHands line = do
  (h1, rest1) <- readHand (skipSpaces line)
  (h2, _) <- readHand (skipSpaces rest1)
  return (h1, h2)

-- Fetch the hands from Problem 54
fetchSets :: IO String
fetchSets = do
  bs <- simpleHttp "https://projecteuler.net/project/resources/p054_poker.txt"
  return $ L.unpack bs

groupedHand :: Hand -> [(Int, Number)]
groupedHand (Hand cards) = groupedNumbers cards

handCards :: Hand -> [Card]
handCards (Hand cards) = cards

problem54 :: IO Int
problem54 = do
  setsString <- fetchSets
  let sets = catMaybes $ readTwoHands <$> lines setsString
  -- let withScores = map (\(a,b) -> (a,b, score a, score b, a > b)) sets
  -- mapM_ print withScores
  let aWins (a, b) = score a > score b
  return $ length $ filter aWins sets

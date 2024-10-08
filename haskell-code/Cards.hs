{-# LANGUAGE TypeApplications #-}
-- | Define the french card game cards with default values
--
-- Yaron Minsky: "Make illegal states impossible"

module Cards (Suit(..), Rank(..), Card(..), prettyCard, deck, compareCards, Player(..), Hand(..)) where

import qualified Data.Set as Set
import Data.Set (Set)

data Suit = Hearts
          | Diamonds
          | Clubs
          | Spades
          deriving (Eq, Ord, Show, Enum, Bounded)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | King
          | Queen
          | Jack
          | Ace
          deriving (Eq, Ord, Enum, Show, Bounded)

-- | deck holds all cards
--
-- >>> length deck
-- 52
deck :: [Card]
deck = Card <$> enumerate <*> enumerate

enumerate :: (Enum a, Bounded a) => [a]
enumerate = enumFrom minBound

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct as bs = (,) <$> as <*> bs

compareCards :: Card -> Card -> Maybe Ordering
compareCards (Card s1 v1) (Card s2 v2) =
  if s1 == s2 then Just $ v1 `compare` v2 else Nothing

prettyRank :: Rank -> String
prettyRank King = "King"
prettyRank Queen = "Queen"
prettyRank Jack = "Jack"
prettyRank Ace = "Ace"
prettyRank v = show (fromEnum v + 2)

data Card = Card {suit :: Suit, rank :: Rank}
  deriving (Eq, Ord, Show)

prettyCard :: Card -> String
prettyCard (Card s v) = prettyRank v <> " of " <> show s

-- | rankValue computes the numeric rankValue of RankValue
--
-- >>> prettyCard $ Card Spades Ten
-- "10 of Spades"
-- >>> rankValue Two
-- 2
rankValue :: Rank -> Int
rankValue v = 2 + fromEnum v

newtype Hand = Hand {unhand :: Set Card}
  deriving Show

makeHand :: [Card] -> Hand
makeHand = Hand . Set.fromList

handCards :: Hand -> [Card]
handCards = Set.toList . unhand

isHandEmpty :: Hand -> Bool
isHandEmpty = Set.null . unhand

containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card $ unhand hand

removeCard :: Card -> Hand -> Hand
removeCard card hand = Hand $ Set.delete card (unhand hand)

emptyHand :: Hand
emptyHand = Hand Set.empty

newtype Player = Player { playerName :: String }
  deriving (Show, Ord, Eq)

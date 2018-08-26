#!/usr/bin/env runhaskell
{- https://projecteuler.net/problem=54
Problem 54
Poker hands

In the card game poker, a hand consists of five cards and are ranked, from
lowest to highest, in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order: 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen,
King, Ace.

If two players have the same ranked hands then the rank made up of the highest
value wins; for example, a pair of eights beats a pair of fives (see example 1
below). But if two ranks tie, for example, both players have a pair of queens,
then highest cards in each hand are compared (see example 4 below); if the
highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:

Hand        Player 1         Player 2        Winner
1        5H 5C 6S 7S KD   2C 3S 8S 8D TD    Player 2
            Pair of Fives   Pair of Eights
2        5D 8C 9S JS AC   2C 5C 7D 8S QH    Player 1
        Highest card Ace Highest card Queen
3        2D 9C AS AH AC   3D 6D 7D TD QD    Player 2
            Three Aces   Flush with Diamonds
4        4D 6S 9H QH QC   3D 6D 7H QD QS    Player 1
            Pair of Queens   Pair of Queens
        Highest card Nine  Highest card Seven
5        2H 2D 4C 4D 4S   3C 3D 3S 9S 9D    Player 1
            Full House       Full House
        With Three Fours  with Three Threes

The file, poker.txt, contains one-thousand random hands dealt to two players.
Each line of the file contains ten cards (separated by a single space): the
first five are Player 1's cards and the last five are Player 2's cards. You
can assume that all hands are valid (no invalid characters or repeated cards),
each player's hand is in no specific order, and in each hand there is a clear
winner.

How many hands does Player 1 win?
-}

import qualified Data.List as DL (sort, sortBy, group, nub)
    
    -- There are four different suits
data Suit = Clubs | Diamonds | Hearts | Spades
        deriving (Show, Read, Eq)

-- A card has one of these 13 ranks
data Rank = Two | Three | Four  | Five
          | Six | Seven | Eight | Nine
          | Ten | Jack  | Queen | King
          | Ace
        deriving (Show, Read, Eq, Ord, Enum)

-- A card has a suit and a value
data Card = Card { suit :: Suit, rank :: Rank }
        deriving (Show, Read, Eq)

-- Valuation of a poker hand
data Valuation = HighCard      { highest :: Rank
                               , kicker :: Rank
                               , kicker2nd :: Rank
                               , kicker3rd :: Rank
                               , kicker4th :: Rank }
               | OnePair       { pair :: Rank
                               , kicker :: Rank
                               , kicker2nd :: Rank
                               , kicker3rd :: Rank }
               | TwoPair       { pairHigh :: Rank
                               , pairLow :: Rank
                               , kicker :: Rank }
               | ThreeOfAKind  { threeKind :: Rank
                               , kicker :: Rank
                               , kicker2nd :: Rank }
               | Straight      { highest :: Rank }
               | Flush         { highest :: Rank
                               , kicker :: Rank
                               , kicker2nd :: Rank
                               , kicker3rd :: Rank
                               , kicker4th :: Rank }
               | FullHouse     { threeKind :: Rank
                               , pair :: Rank }
               | FourOfAKind   { fourKind :: Rank
                               , kicker :: Rank }
               | StraightFlush { highest :: Rank }
               | RoyalFlush
               deriving (Show, Read, Eq, Ord)

-- Determine if a list of cards is a flush.  This is the case
-- when all cards have the same suit.
isFlush :: [Card] -> Bool
isFlush = allEqual . (map suit)
        where allEqual (x:xs) = all ((==) x) xs
              allEqual _      = True

-- Determine if all cards in the hand have consecutive Ranks
isStraight :: [Card] -> Bool
isStraight = isStraight' . DL.sort . (map rank)
        where isStraight' xss = hasNoDuplicates xss && hasNoGaps xss
              hasNoDuplicates xss = xss == DL.nub xss
              hasNoGaps xss@(x:xs) = and (zipWith (==) (map pred xs) xss)

-- This input: [Card Diamonds Ten, Card Diamonds Ten,
--              Card Diamonds Ten, Card Diamonds Eight,
--              Card Diamonds Eight]
-- Yields:     [(3,Ten),(2,Eight)]
sortedGroups :: [Card] -> [(Int, Rank)]
sortedGroups = reverse . DL.sort . map (\x -> (length x, head x)) . DL.group . DL.sort . (map rank)

-- Determine the valuation of a hand
evaluate :: [Card] -> Valuation
evaluate hand
        -- The RoyalFlush is effectively redundant. Comparing a StraightFlush
        -- with another StraightFlush will always correctly determine the winner.
        | straight && flush && hasAce = RoyalFlush
        | straight && flush           = StraightFlush r1
        | head kinds == 4             = FourOfAKind   r1 r2
        | take 2 kinds == [3, 2]      = FullHouse     r1 r2
        | flush                       = Flush         r1 r2 r3 r4 r5
        | straight                    = Straight      r1
        | head kinds == 3             = ThreeOfAKind  r1 r2 r3
        | take 2 kinds == [2, 2]      = TwoPair       r1 r2 r3
        | head kinds == 2             = OnePair       r1 r2 r3 r4
        | otherwise                   = HighCard      r1 r2 r3 r4 r5
        where groups = sortedGroups hand
              flush = isFlush hand
              straight = isStraight hand
              kinds = map fst groups
              ranks = map snd groups
              hasAce = head ranks == Ace
              r1 = (ranks !! 0)
              r2 = (ranks !! 1)
              r3 = (ranks !! 2)
              r4 = (ranks !! 3)
              r5 = (ranks !! 4)

-- Parse a suit
strToSuit :: Char -> Suit
strToSuit suit
    | suit == 'C' = Clubs
    | suit == 'H' = Hearts
    | suit == 'S' = Spades
    | suit == 'D' = Diamonds
    | otherwise   = error "Parse error: invalid suit."

-- Parse a character
strToRank :: Char -> Rank
strToRank rank
    | rank == '2' = Two
    | rank == '3' = Three
    | rank == '4' = Four
    | rank == '5' = Five
    | rank == '6' = Six
    | rank == '7' = Seven
    | rank == '8' = Eight
    | rank == '9' = Nine
    | rank == 'T' = Ten
    | rank == 'J' = Jack
    | rank == 'Q' = Queen
    | rank == 'K' = King
    | rank == 'A' = Ace
    | otherwise   = error "Parse error: invalid rank."

-- Turns a two character string into a Card.
strToCard :: [Char] -> Card
strToCard (rank:suit:[]) = Card (strToSuit suit) (strToRank rank)
strToCard _ = error "Parse error: invalid card."

-- Compare hands and return the winning player
determineWinner :: ([Card], [Card]) -> Int
determineWinner (hand1, hand2)
    | score1 > score2 = 1
    | score1 < score2 = 2
    | otherwise       = error "No winner."
    where (score1, score2) = (evaluate hand1, evaluate hand2)

-- Read the contents of a file and turn them into a list of games.
parseGames :: String -> [([Card], [Card])]
parseGames = map (splitAt 5 . map strToCard . words) . lines

-- Entrypoint
main :: IO ()
main = do file <- readFile "resources/p054_poker.txt"
          print $ length $ filter (== 1) $ map determineWinner $ parseGames file

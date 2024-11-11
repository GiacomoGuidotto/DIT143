module Lab2.BlackJack
  ( hand2,
    sizeSteps,
    display,
    value,
    gameOver,
    winner,
    prop_onTopOf_assoc,
    prop_size_onTopOf,
    fullDeck,
  )
where

import Lab2.Cards
import Lab2.RunGame

-- | Computing the steps of the size function for an example hand
hand2 :: Hand
hand2 =
  Add
    (Card (Numeric 2) Hearts)
    (Add (Card Jack Spades) Empty)

sizeSteps :: [Integer]
sizeSteps =
  [ size hand2,
    size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)),
    1 + size (Add (Card Jack Spades) Empty),
    1 + 1 + size Empty,
    1 + 1 + 0,
    2
  ]

-- | A1. Displaying function -----------------------------------------

-- | Display the rank of a card, removing the constructor
displayRank :: Rank -> String
displayRank (Numeric n) = show n
displayRank r = show r

-- | Display the suit of a card, using unicode symbols
displaySuit :: Suit -> String
displaySuit Hearts = "♥"
displaySuit Spades = "♠"
displaySuit Diamonds = "♦"
displaySuit Clubs = "♣"

-- | Display a card
displayCard :: Card -> String
displayCard (Card r s) = displayRank r ++ " of " ++ displaySuit s

-- | Display a hand
display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ "\n" ++ display h

-- | A2. Value function ----------------------------------------------

-- | Compute the value of a rank with aces counting for 1
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace = 1
valueRank _ = 10

-- | Compute the value of a hand and count the number of aces
initalValue :: Hand -> (Integer, Integer)
initalValue Empty = (0, 0)
initalValue (Add c h) = (valueRank (rank c) + v, ace + a)
  where
    (v, a) = initalValue h
    ace = if rank c == Ace then 1 else 0

-- | Compute the value of a hand and adjust the value if the hand
-- contains aces
value :: Hand -> Integer
value h =
  let extra = min a ((21 - v) `div` 10)
   in v + 10 * extra
  where
    (v, a) = initalValue h

-- | A3. Game over function -----------------------------------------

-- | Check if the player is bust
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- | A4. Winner function ---------------------------------------------

-- | Compute the winner of the game
winner :: Hand -> Hand -> Player
winner g b
  | gameOver g = Bank
  | gameOver b = Guest
  | value g > value b = Guest
  | otherwise = Bank

-- | B1. Put on Top function ----------------------------------------

-- | Put the first hand on top of the second hand
(<+) :: Hand -> Hand -> Hand
Empty <+ h = h
(Add c h1) <+ h2 = Add c (h1 <+ h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
  p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 =
  (size (h1 <+ h2) :: Integer) == (size h1 + size h2 :: Integer)

-- | B2. Full deck function -----------------------------------------

-- | Compute a full deck of cards
fullDeck :: Hand
fullDeck = foldr Add Empty [Card r s | s <- suits, r <- ranks]
  where
    ranks = [Numeric n | n <- [2 .. 10]] ++ [Jack, Queen, King, Ace]
    suits = [Hearts, Spades, Diamonds, Clubs]

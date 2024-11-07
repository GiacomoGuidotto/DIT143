module Lab2.BlackJack where

import Lab2.Cards
import Lab2.RunGame
import Test.QuickCheck

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

module Main (main) where

import Lab2.BlackJack
import Lab2.Cards

hand1 :: Hand
hand1 =
  Add
    (Card Ace Hearts)
    (Add (Card Ace Spades) Empty)

testHand :: Hand -> IO ()
testHand h = do
  putStr (display h)
  putStr " value: "
  print (value h)
  putStr "\n"

main :: IO ()
main = do
  putStr "sizeSteps:\n"
  print sizeSteps
  putStr "hand1:\n"
  testHand hand1
  putStr "hand2:\n"
  testHand hand2

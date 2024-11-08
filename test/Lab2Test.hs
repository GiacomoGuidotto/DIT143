module Main (main) where

import Lab2.BlackJack
import Lab2.Cards

winningHand :: Hand
winningHand =
  Add
    (Card Ace Hearts)
    (Add (Card Jack Spades) Empty)

testHand :: Hand -> IO ()
testHand h = do
  putStr (display h)
  putStr "- value: "
  print (value h)
  putStr "- gameOver: "
  print (gameOver h)

main :: IO ()
main = do
  putStr "sizeSteps:\n"
  print sizeSteps
  putStr "\nwinningHand:\n"
  testHand winningHand
  putStr "\nhand2:\n"
  testHand hand2
  putStr "\nwinner: "
  print (winner hand2 winningHand)

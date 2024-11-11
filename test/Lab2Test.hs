module Main (main) where

import Lab2.BlackJack
import Lab2.Cards
import Test.QuickCheck

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
  putStrLn "=== Manual Tests ==="
  putStr "sizeSteps:\n"
  print sizeSteps
  putStr "\nwinningHand:\n"
  testHand winningHand
  putStr "\nhand2:\n"
  testHand hand2
  putStr "\nwinner hand2 winningHand:\n"
  print (winner hand2 winningHand)
  putStr "\nfullDeck:\n"
  putStr $ display fullDeck
  putStrLn "=== QuickCheck Tests ==="
  quickCheck prop_onTopOf_assoc
  quickCheck prop_size_onTopOf

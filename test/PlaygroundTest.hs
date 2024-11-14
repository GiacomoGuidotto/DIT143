module Main (main) where

import Playground

testCollatz :: IO ()
testCollatz = do
  print (collatz 3)

testIO :: IO ()
testIO =
  do
    print ex
    >> print (splitUp ex)
    >> print (partition (>= 0) ex)
    >> print (partition' (>= 0) ex)
    >> print (partition'' (>= 0) ex)
  where
    ex = [1, -2, 3, -4, 5, -6, 7, -8, 9, -10]

main :: IO ()
-- main = testCollatz
-- main = testIO
main = hangman

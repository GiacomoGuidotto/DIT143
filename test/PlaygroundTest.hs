module Main (main) where

import Playground

main :: IO ()
main = do
  print (collatz 3)

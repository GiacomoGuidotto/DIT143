module Main (main) where

import Lab3.Sudoku
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
  putStrLn "=== QuickCheck tests ==="
  quickCheck prop_Sudoku
  quickCheck prop_blocks_lengths
  putStrLn "=== Example ==="
  printSudoku example
  print (blocks example)
  print (isOkay example)

module Main (main) where

import Lab3.Sudoku
import Test.QuickCheck (quickCheck)

test :: IO ()
test = do
  putStrLn "=== Example ==="
  s <- readSudoku "sudoku_test.txt"
  printSudoku s
  print (isOkay s)
  print (blocks s)

main :: IO ()
main = do
  putStrLn "=== QuickCheck tests ==="
  quickCheck prop_Sudoku
  quickCheck prop_blocks_lengths
  quickCheck prop_blanks_allBlanks
  quickCheck prop_bangBangEquals_correct
  quickCheck prop_update_updated

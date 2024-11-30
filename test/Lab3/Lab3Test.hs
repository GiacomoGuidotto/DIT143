module Main (main) where

import Sudoku
import Test.QuickCheck (Testable, maxSuccess, quickCheck, quickCheckWith, stdArgs)

fewerChecks :: (Testable prop) => prop -> IO ()
fewerChecks = quickCheckWith stdArgs {maxSuccess = 10}

test :: FilePath -> IO ()
test p = do
  putStr "=== Testing " >> putStr p >> putStrLn " ==="
  s <- readSudoku p
  printSudoku s
  putStr "is okay: " >> print (isOkay s)
  putStrLn "solution:" >> readAndSolve p >> putStrLn ""

main :: IO ()
main = do
  test "src/Lab3/assets/broken_sudoku.su"
  test "src/Lab3/assets/test_sudoku.su"
  test "src/Lab3/assets/impossible_sudoku.su"

  putStrLn "=== QuickCheck tests ==="
  quickCheck prop_Sudoku
  quickCheck prop_blocks_lengths
  quickCheck prop_blanks_allBlanks
  quickCheck prop_bangBangEquals_correct
  quickCheck prop_update_updated
  fewerChecks prop_SolveSound

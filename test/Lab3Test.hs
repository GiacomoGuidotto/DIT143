module Main (main) where

import Lab3.Sudoku

main :: IO ()
main = do
  s <- readSudoku "test.txt"
  printSudoku s

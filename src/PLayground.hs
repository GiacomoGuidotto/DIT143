{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore #-}

module Playground where

import Data.List (union, (\\))
import System.Random

collatz :: Integer -> [Integer]
collatz n
  | n == 1 = [1]
  | even n = n : collatz (div n 2)
  | odd n = n : collatz (3 * n + 1)
  | otherwise = error "collatz: invalid argument"

-- Higher-order functions

-- split a list into two lists, one with all non-negative numbers and
-- one with all negative numbers
splitUp :: (Num n, Ord n) => [n] -> ([n], [n])
splitUp xs = (filter (>= 0) xs, filter (< 0) xs)

-- partition a list into two lists, one with all elements that satisfy a
-- predicate and one with all elements that do not satisfy the predicate
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x : xs)
  | p x = (x : ys, zs)
  | otherwise = (ys, x : zs)
  where
    (ys, zs) = partition p xs

-- alternative implementations of partition
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f xs = (filter f xs, filter (\x -> not (f x)) xs)

partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' f xs = (filter f xs, filter (not . f) xs)

-- alternative implementation of splitUp using partition
splitUp' :: (Num n, Ord n) => [n] -> ([n], [n])
splitUp' = partition (>= 0)

-- IO

-- copy the contents of a list of files into a single file
copyAll :: [FilePath] -> FilePath -> IO ()
copyAll fromFiles toFile = do
  contents <- sequence $ map readFile fromFiles
  writeFile toFile (concat contents)

-- for loop in IO env
forLoop :: [a] -> (a -> IO ()) -> IO ()
forLoop xs f = sequence_ $ map f xs

-- Hangman game
dict :: String
dict = "/usr/share/dict/words"

maxGuesses :: Int
maxGuesses = 10

hangman :: IO ()
hangman = do
  word <- randomWord
  gameLoop word ""

randomWord :: IO String
randomWord = do
  contents <- readFile dict
  let ws = lines contents
  i <- randomRIO (0, length ws - 1)
  return (ws !! i)

gameLoop :: String -> String -> IO ()
gameLoop w g
  | win = showWin
  | lose = showLose
  | otherwise = do
      showStatus
      guesses <- getLine
      gameLoop w (g `union` take lives guesses)
  where
    win = all (`elem` g) w
    lose = lives <= 0
    lives = maxGuesses - length (g \\ w)
    showWin = putStrLn $ "Win! " ++ w
    showLose = putStrLn $ "Lose! " ++ w
    showStatus = do
      putStrLn [if c `elem` g then c else '_' | c <- w]
      putStrLn $ "Type your guesses (" ++ show lives ++ " lives remaining)"

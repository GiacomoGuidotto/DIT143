module Main (main) where

import Lab1
import Test.QuickCheck

main :: IO ()
main = quickCheck prop_powers'

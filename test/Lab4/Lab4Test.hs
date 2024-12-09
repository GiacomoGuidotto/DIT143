{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Test.QuickCheck

-- a Template Haskell quirk that allows the splice to run in the current module
return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  allPassed <- runTests
  if allPassed
    then putStrLn "All tests passed!"
    else putStrLn "Some tests failed."

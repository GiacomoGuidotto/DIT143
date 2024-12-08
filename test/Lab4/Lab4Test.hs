module Main (main) where

import Expr
import Test.QuickCheck

main :: IO ()
main = do
  verboseCheck prop_ShowReadExpr

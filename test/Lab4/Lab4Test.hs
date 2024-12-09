module Main (main) where

import Expr
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck prop_ShowReadExpr
  quickCheck prop_simplifyNoVar
  quickCheck prop_simplifyPreservesValue
  quickCheck prop_simplifyBasicOps

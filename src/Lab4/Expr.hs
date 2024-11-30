module Expr where

import Test.QuickCheck

data Expr
  = Num Double
  | Var
  | Op (Double -> Double -> Double) Expr Expr
  | Function (Double -> Double) Expr

instance Arbitrary Expr where
  arbitrary = undefined

x :: Expr
x = undefined

num :: Double -> Expr
num = undefined

add, mul :: Expr -> Expr -> Expr
add = undefined
mul = undefined

sin, cos :: Expr -> Expr
sin = undefined
cos = undefined

size :: Expr -> Int
size = undefined

showExpr :: Expr -> String
showExpr = undefined

eval :: Expr -> Double -> Double
eval = undefined

readExpr :: String -> Maybe Expr
readExpr = undefined

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined

arbExpr :: Int -> Gen Expr
arbExpr = undefined

simplify :: Expr -> Expr
simplify = undefined

differentiate :: Expr -> Expr
differentiate = undefined
